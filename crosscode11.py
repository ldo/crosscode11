#+
# A machine-code generation library for the PDP-11.
#
# Written by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#-

import enum
import struct

class o :
    """container for operand objects."""

    def __add__(self, offset) :
        """allows convenient r+offset notation."""
        assert self.hasoffset, "offset not allowed on this operand"
        if type(offset) == int :
            return (self, offset)
        else :
            return NotImplemented
        #end if
    #end __add__

    __radd__ = __add__

    def __sub__(self, offset) :
        """allows convenient r-offset notation."""
        assert self.hasoffset, "offset not allowed on this operand"
        if type(offset) == int :
            return (self, 0o177777 ^ offset)
        else :
            return NotImplemented
        #end if
    #end __sub__

    def __init__(self, reg, ind, postinc, predec, hasoffset, bitpat) :
        self.reg = reg # register nr [0 .. 7]
        self.ind = ind # indirect (deferred)
        self.postinc = postinc
        self.predec = predec
        self.hasoffset = hasoffset
        self.bitpat = bitpat
    #end __init__

    # Valid operand forms are defined as publicly-visible attributes of
    # this class. Operands requiring offset fields are represented as
    # 2-tuples, the first element being an operand object and the second
    # being the integer offset.

#end o

for \
        name, reg, doindir \
    in \
        (
            ("R0", 0, True),
            ("R1", 1, True),
            ("R2", 2, True),
            ("R3", 3, True),
            ("R4", 4, True),
            ("R5", 5, True),
            ("R6", 6, True),
            ("SP", 6, True),
            ("R7", 7, False),
            ("PC", 7, False),
        ) \
:
    # names are
    #     o.Rn -- register direct
    #     o.aRn -- register deferred
    #     o.Rni -- register post-increment
    #     o.aRni -- register post-increment deferred
    #     o.pRn -- register pre-decrement
    #     o.apRn -- register pre-decrement deferred
    #     o.Rno -- register + offset
    #     o.aRno -- register + offset deferred
    setattr(o, name, o(reg, False, False, False, False, reg))
    setattr(o, "a" + name, o(reg, True, False, False, False, 0o10 | reg))
    setattr(o, name + "i", o(reg, False, True, False, not doindir, 0o20 | reg))
    setattr(o, "a" + name + "i", o(reg, True, True, False, not doindir, 0o30 | reg))
    if doindir :
        # PC auto-decrement not supported
        setattr(o, "p" + name, o(reg, False, False, True, False, 0o40 | reg))
        setattr(o, "ap" + name, o(reg, True, False, True, False, 0o50 | reg))
    #end if
    setattr(o, name + "o", o(reg, False, False, False, True, 0o60 | reg))
    setattr(o, "a" + name + "o", o(reg, True, False, False, True, 0o70 | reg))
#end for
setattr(o, "i", o(7, False, True, False, True, 0o27)) # for immediate addressing
setattr(o, "a", o(7, True, True, False, True, 0o37)) # for absolute addressing
# immediate operands handled specially
del reg, doindir

class cc :
    """condition codes flags bits."""
    C = 1
    V = 2
    Z = 4
    N = 8
#end cc

class op :
    """container for instruction objects."""

    # begin internal operand-validation utilities. Each of these takes an
    # object of class o (above), and a bit offset into the opcode word at
    # which to insert the operand field. The result is a 2-tuple,
    # the first element being a mask to inclusive-or with the opcode word
    # to insert the operand descriptor field, the second element being the
    # integer number of additional operand words expected.

    @staticmethod
    def regonly(opnd, offset) :
        # validates a register-only operand.
        assert (opnd.bitpat & 0o70) == 0, "register operand only"
        return (opnd.reg << offset, 0)
    #end regonly

    @staticmethod
    def general(opnd, offset) :
        # validates a general operand.
        return (opnd.bitpat << offset, (0, 2)[opnd.hasoffset])
    #end general

    # end internal operand-validation utilities

    # begin internal instruction-construction utilities. Each of these takes
    # a basic opcode word with all relevant operand descriptor fields initialized
    # to zero, plus 0 .. 2 additional arguments being operands of class o (above).
    # The result is a 2-tuple, the first element being the instruction opcode
    # word with all operand descriptor fields filled in, and the second being
    # the integer number of additional operand words expected.

    @staticmethod
    def branchonly(bitpat, opnd) :
        # destination for branch instruction
        assert isinstance(opnd, int), "branch offset must be integer"
        assert opnd >= -128 and opnd < 128, "branch offset outside valid range"
        return (bitpat | (opnd & 255), 0)
    #end branchonly

    @staticmethod
    def regandbranch(bitpat, opnd1, opnd2) :
        # operands for SOB instruction
        operand1 = op.regonly(opnd1, 6)
        assert isinstance(opnd2, int), "operand2 must be integer"
        assert opnd2 >= 0 and opnd2 < 64, "branch offset outside valid range"
        return (bitpat | operand1[0] | (opnd & 63), operand1[1])
    #end regandbranch

    @staticmethod
    def byteoperand(bitpat, opnd) :
        # literal byte operand for EMT and TRAP instructions
        assert isinstance(opnd, int), "operand must be integer"
        assert opnd >= 0 and opnd < 256, "byte operand outside valid range"
        return (bitpat | (opnd & 255), 0)
    #end byteoperand

    @staticmethod
    def priooperand(bitpat, opnd) :
        # literal priority for SPL instruction
        assert isinstance(opnd, int), "operand must be integer"
        assert opnd >= 0 and opnd < 8, "priority outside valid range"
        return (bitpat | (opnd & 7), 0)
    #end priooperand

    @staticmethod
    def regoperand(bitpat, opnd) :
        # single register operand for RTS/FADD/FSUB/FMUL/FDIV
        operand = op.regonly(opnd, 0)
        return (bitpat | operand[0], operand[1])
    #end regoperand

    @staticmethod
    def markoperand(bitpat, opnd) :
        # literal integer operand for MARK instruction
        assert isinstance(opnd, int), "operand must be integer"
        assert opnd >= 0 and opnd < 64, "mark count outside valid range"
        return (bitpat | (opnd & 63), 0)
    #end markoperand

    @staticmethod
    def nooperand(bitpat) :
        # instruction with no operands
        return (bitpat, 0)
    #end nooperand

    @staticmethod
    def singleoperand(bitpat, opnd) :
        # instruction with single general operand
        operand = op.general(opnd, 0)
        return (bitpat | operand[0], operand[1])
    #end singleoperand

    @staticmethod
    def condoperand(bitpat, opnd = None) :
        # instruction with condition-code operand (defaulting to true if none)
        if opnd != None :
            assert isinstance(opnd, int), "operand must be integer"
            assert opnd >= 0 and opnd < 16, "condition flag mask outside valid range"
        else :
            opnd = 15 # default to all flags
        #end if
        return (bitpat | opnd, 0)
    #end condoperand

    @staticmethod
    def doubleoperand(bitpat, opnd1, opnd2) :
        # instruction with two general operands
        operand1 = op.general(opnd1, 6)
        operand2 = op.general(opnd2, 0)
        return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
    #end doubleoperand

    @staticmethod
    def opandreg(bitpat, opnd1, opnd2) :
        # instruction with one general and one register operand, in that order (MUL, DIV, ASH, ASHC)
        operand1 = op.general(opnd1, 0)
        operand2 = op.regonly(opnd2, 6)
        return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
    #end opandreg

    @staticmethod
    def regandop(bitpat, opnd1, opnd2) :
        # instruction with one register and one general operand, in that order (XOR, JSR)
        operand1 = op.regonly(opnd1, 6)
        operand2 = op.general(opnd2, 0)
        return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
    #end regandop

    # end internal instruction-construction utilities

    def __init__(self, bitpat, nroperands, genmask, instr) :
        self.bitpat = bitpat
        self.nroperands = nroperands
        self.genmask = genmask # mask for which operands are general
        self.instr = instr
    #end __init__

    def __call__(self, *operands) :
        return self.instr(*(self.bitpat,) + operands)
    #end __call__

    # All the valid instructions are defined (below) as publicly-visible
    # attributes of this class. The value of each is a function that takes
    # instruction operands as arguments, and returns a 2-tuple, with item 0
    # being the value of the first (or only) instruction word, and item 1 being
    # the number of subsequent operand words.

#end op

for \
        name, bitpat \
    in \
        (
            ("CLR", 0o005000),
            ("COM", 0o005100),
            ("INC", 0o005200),
            ("DEC", 0o005300),
            ("NEG", 0o005400),
            ("ADC", 0o005500),
            ("SBC", 0o005600),
            ("TST", 0o005700),
            ("ROR", 0o006000),
            ("ROL", 0o006100),
            ("ASR", 0o006200),
            ("ASL", 0o006300),
        ) \
:
    setattr(op, name, op(bitpat, 1, 1, op.singleoperand))
    setattr(op, name + "B", op(bitpat | 0o100000, 1, 1, op.singleoperand))
#end for
setattr(op, "SWAB", op(0o00300, 1, 1, op.singleoperand))
setattr(op, "SXT", op(0o006700, 1, 1, op.singleoperand))
for \
        name, bitpat \
    in \
        (
            ("MOV", 0o10000),
            ("CMP", 0o20000),
            ("CMP", 0o20000),
            ("BIT", 0o30000),
            ("BIC", 0o40000),
            ("BIS", 0o50000),
        ) \
:
    setattr(op, name, op(bitpat, 2, 3, op.doubleoperand))
    setattr(op, name + "B", op(bitpat | 0o100000, 2, 3, op.doubleoperand))
#end for
setattr(op, "ADD", op(0o060000, 2, 3, op.doubleoperand))
setattr(op, "SUB", op(0o160000, 2, 3, op.doubleoperand))
for \
        name, bitpat \
    in \
        (
            ("MUL", 0o70000),
            ("DIV", 0o71000),
            ("ASH", 0o72000),
            ("ASHC", 0o73000),
        ) \
:
    setattr(op, name, op(bitpat, 2, 3, op.opandreg))
#end for
setattr(op, "XOR", op(0o74000, 2, 3, op.regandop))
for \
        name, bitpat \
    in \
        (
            ("BR", 0o000400),
            ("BNE", 0o001000),
            ("BEQ", 0o001400),
            ("BPL", 0o100000),
            ("BMI", 0o100400),
            ("BVC", 0o102000),
            ("BVS", 0o102400),
            ("BCC", 0o103000),
            ("BCS", 0o103400),
            ("BGE", 0o002000),
            ("BLT", 0o002400),
            ("BGT", 0o03000),
            ("BLE", 0o03400),
            ("BHI", 0o101000),
            ("BLOS", 0o101400),
            ("BHIS", 0o103000),
            ("BLO", 0o103400),
        ) \
:
    setattr(op, name, op(bitpat, 1, 0, op.branchonly))
#end for
setattr(op, "JMP", op(0o00100, 1, 1, op.singleoperand))
setattr(op, "JSR", op(0o04000, 2, 3, op.regandop))
setattr(op, "RTS", op(0o00200, 1, 1, op.regoperand))
setattr(op, "MARK", op(0o06400, 1, 0, op.markoperand))
setattr(op, "SOB", op(0o77000, 2, 0, op.regandbranch))
setattr(op, "EMT", op(0o104000, 1, 0, op.byteoperand))
setattr(op, "TRAP", op(0o104400, 1, 0, op.byteoperand))
setattr(op, "BPT", op(0o00003, 0, 0, op.nooperand))
setattr(op, "IOT", op(0o00004, 0, 0, op.nooperand))
setattr(op, "RTI", op(0o00002, 0, 0, op.nooperand))
setattr(op, "RTT", op(0o00006, 0, 0, op.nooperand))
setattr(op, "SPL", op(0o00230, 1, 0, op.priooperand))
setattr(op, "HALT", op(0o00000, 0, 0, op.nooperand))
setattr(op, "WAIT", op(0o00001, 0, 0, op.nooperand))
setattr(op, "RESET", op(0o00005, 0, 0, op.nooperand))
for \
        name, bitpat \
    in \
        (
            ("MFPI", 0o006500),
            ("MTPI", 0o006600),
            ("MFPD", 0o106500),
            ("MTPD", 0o106600),
        ) \
:
    setattr(op, name, op(bitpat, 1, 1, op.singleoperand))
#end for
for name in dir(cc) :
    if name[0] != "_" :
        setattr(op, "CL" + name, op(0o00240 | getattr(cc, name), 0, 0, op.nooperand))
        setattr(op, "SE" + name, op(0o00260 | getattr(cc, name), 0, 0, op.nooperand))
    #end if
#end for
setattr(op, "CCC", op(0o00240, 1, 0, op.condoperand))
setattr(op, "SCC", op(0o00260, 1, 0, op.condoperand))
setattr(op, "NOP", op(0o00240, 0, 0, op.nooperand))
# FIS:
setattr(op, "FADD", op(0o750000, 1, 1, op.regoperand))
setattr(op, "FSUB", op(0o750010, 1, 1, op.regoperand))
setattr(op, "FMUL", op(0o750020, 1, 1, op.regoperand))
setattr(op, "FDIV", op(0o750030, 1, 1, op.regoperand))
del name, bitpat
# FPP, CIS NYI

class CodeBuffer :
    """overall management of a block of generated code."""

    class Memory :
        """a block of memory contents."""

        def __init__(self) :
            self.blocks = [[] * 8] # contiguous sequences of defined memory content bytes
              # length must exactly divide 65536
            self.baseaddrs = [None] * len(self.blocks)
              # address corresponding to start of each block
        #end __init__

        def eb(self, addr) :
            """returns the byte at the specified adddress."""
            (index, offset) = divmod(addr, (65536 // len(self.blocks)))
            if (
                    self.baseaddrs[index] != None
                and
                    addr >= self.baseaddrs[index]
                and
                    addr < self.baseaddrs[index] + len(self.blocks[index])
            ) :
                result = self.blocks[index][addr - self.baseaddrs[index]]
            else :
                result = 0
            #end if
            return \
                result
        #end eb

        def ew(self, addr) :
            """returns the word at the specified address (must be even)."""
            assert (addr & 1) == 0, "address must be even"
            return self.eb(addr) | self.eb(addr + 1) << 8
        #end ew

        def db(self, addr, value) :
            """sets the byte at the specified address to the specified value."""
            (index, offset) = divmod(addr, (65536 // len(self.blocks)))
            incr = 256 # increment block sizes by whole multiples of this
            if self.baseaddrs[index] == None or self.baseaddrs[index] > addr :
                newaddr = addr // incr * incr
                if self.baseaddrs[index] == None :
                    self.baseaddrs[index] = newaddr + incr # just to preallocate first block
                #end if
                self.blocks[index][0:0] = [0] * (self.baseaddrs[index] - newaddr)
                self.baseaddrs[index] = newaddr
            elif self.baseaddrs[index] + len(self.blocks[index]) <= addr :
                newlen = ((addr - self.baseaddrs[index]) + incr) // incr * incr
                self.blocks[index].extend([0] * (newlen - len(self.blocks[index])))
            #end if
            self.blocks[index][addr - self.baseaddrs[index]] = value
            return self # for convenient chaining of calls
        #end db

        def dw(self, addr, value) :
            """sets the word at the specified address (must be even) to the specified value."""
            assert (addr & 1) == 0, "address must be even"
            return self.db(addr, value & 255).db(addr + 1, value >> 8 & 255)
        #end dw

        def dumpb(self, start = None, end = None) :
            """generator which yields a sequence of (addr, value) pairs for nonzero byte
            values in order of increasing address over the specified range."""
            for index in range(0, len(self.blocks)) :
                base = self.baseaddrs[index]
                block = self.blocks[index]
                for offset in range(0, len(block)) :
                    addr = base + offset
                    value = block[offset]
                    if (start == None or addr >= start) and (end == None or addr < end) and value != 0 :
                        yield (addr, value)
                    #end if
                #end for
            #end for
        #end dumpb

        def dumpw(self, start = None, end = None) :
            """generator which yields a sequence of (addr, value) pairs for nonzero word
            values in order of increasing address over the specified range. start and end
            must be even."""
            assert start == None or (start & 1) == 0
            assert end == None or (end & 1) == 0
            dump = self.dumpb(start, end)
            lastaddr = None # no saved even byte
            while True :
                try :
                    (addr, value) = next(dump)
                except StopIteration :
                    addr = None
                #end try
                if addr != None :
                    if lastaddr != None :
                        if addr == lastaddr + 1 :
                            # lastaddr was even byte, this is odd byte,
                            # yield combined word
                            yield (lastaddr, lastvalue | value << 8)
                            value = None # already yielded
                        else : # addr > lastaddr + 1
                            yield (lastaddr, lastvalue) # saved even byte had no odd byte
                        #end if
                        lastaddr = None
                    #end if
                    if value != None : # not yet yielded
                        if (addr & 1) != 0 :
                            yield (addr & ~1, value << 8) # even byte is zero
                        else :
                            (lastaddr, lastvalue) = (addr, value)
                              # hold back even byte in case I see odd byte
                        #end if
                    #end if
                else :
                    if lastaddr != None :
                        yield (lastaddr, lastvalue) # saved even byte had no odd byte
                    #end if
                    break
                #end if
            #end while
        #end dumpw

    #end Memory

    class Relocation :
        "note all relocations operate on entire words."

        __slots__ = \
            (
                "psect",
                "offset",
                "reltype",
                "displaced",
                "name",
                "constant",
                "operands",
            )

        @enum.unique
        class RELTYPE(enum.Enum) :
            "relocation types. Note that displaced relocation is indicated by" \
            " the separate displaced flag."
            INTERNAL = 1
            GLOBAL = 2
            GLOBAL_ADDITIVE = 5
            PSECT = 10
            PSECT_ADDITIVE = 13
            COMPLEX = 15
        #end RELTYPE

        class ComplexOp :

            __slots__ = \
                (
                    "op",
                    "name",
                    "constant",
                )

            @enum.unique
            class OP(enum.Enum) :
                "operator types for complex relocation."
                NOP = 0 # no operands, no result
                ADD = 1 # 2 operands
                SUB = 2 # 2 operands
                MUL = 3 # 2 operands
                DIV = 4 # 2 operands
                AND = 5 # 2 operands
                OR = 6 # 2 operands
                XOR = 7 # 2 operands
                NEG = 8 # 1 operand
                NOT = 9 # 1 operand
                # store-result is implicit at end of sequence
                STORE = 10 # 1 operand, no result, terminate
                STORE_DISP = 11 # 1 operand, no result, terminate
                FETCH_GLOBAL = 14 # 1 result
                FETCH_REL = 15 # 1 result
                FETCH_CONST = 16 # 1 result
            #end OP

            def __init__(self, op, name = None, constant = None) :
                assert isinstance(op, self.__class__.OP)
                self.op = op
                self.name = name
                self.constant = constant
            #end __init__

        #end ComplexOp

        def __init__(self, psect, offset, reltype, displaced, name = None, constant = None, operands = None) :
            Relocation = self.__class__ # or perhaps CodeBuffer.Relocation
            assert isinstance(psect, CodeBuffer.PsectClass)
            self.psect = psect
            self.offset = offset
            assert isinstance(reltype, Relocation.RELTYPE)
            assert isinstance(displaced, bool)
            assert (reltype == Relocation.RELTYPE.COMPLEX) == (operands != None)
            assert (reltype == Relocation.RELTYPE.COMPLEX) <= (name == None and constant == None)
            self.reltype = reltype
            self.displaced = displaced
            self.name = name
            self.constant = constant
            assert \
              (
                    operands == None
                or
                    all
                      (
                        isinstance(r, Relocation.ComplexOp)
                        for r in operands
                      )
              )
            self.operands = operands
        #end __init__

    #end Relocation

    class LabelClass :
        "representation of a label within the CodeBuffer. Don’t instantiate directly;" \
        " get one from CodeBuffer.label or CodeBuffer.extlabel."

        # types of label references:
        b16a = 0 # 16-bit absolute byte reference
        b16r = 1 # 16-bit signed relative byte reference
        w8 = 2 # 8-bit signed relative word reference (branch instr)
        w6 = 3 # 6-bit negated word reference (sob instr)

        def __init__(self, name, parent, psect, globlabel, extlabel, weak) :
            self.refs = []
            self.name = name
            self.value = (None, 0)[extlabel] # to begin with
            self.parent = parent
            self.psect = psect
            self.globlabel = globlabel
            self.extlabel = extlabel
            self.weak = weak
            self.rel = None # to begin with
            assert not globlabel or limitsym(name).decode() == name, \
                "globlabel name out of symbol range"
        #end __init__

        def resolve(self, value = None) :
            self.parent.resolve(self, value)
            return self # for convenient chaining of calls
        #end resolve

        def resolved(self) :
            """returns True iff the label has been resolved."""
            return self.value != None
        #end resolved

        def assert_resolved(self) :
            """asserts that the label has been resolved."""
            if self.value == None :
                raise AssertionError("label \"%s\" not resolved" % self.name)
            #end if
        #end assert_resolved

        def __add__(self, reg) :
            """allows convenient r+offset notation."""
            if isinstance(reg, o) :
                assert reg.hasoffset, "offset not allowed on this operand"
                return (reg, self)
            else :
                return NotImplemented
            #end if
        #end __add__

        __radd__ = __add__

        def __sub__(self, other) :
            if isinstance(other, self.parent.LabelClass) and other.psect == self.psect and self.resolved() and other.resolved() :
                # handling unresolved cases NYI
                return self.value - other.value
            else :
                return NotImplemented
            #end if
        #end __sub__

    #end LabelClass

    class PsectClass :
        """representation of a program section within the CodeBuffer. Besides
        allowing logical grouping of code and data sections, I also provide
        automatic checking that psects don't run into each other."""

        known_attrs = \
            { # mapping from psect attribute name to my object attribute and attribute value
                "i" : ("data", False),
                "d" : ("data", True),
                "lcl" : ("gbl", False),
                "gbl" : ("gbl", True),
                "abs" : ("rel", False),
                "rel" : ("rel", True),
                "con" : ("overlay", False),
                "ovr" : ("overlay", True),
            }

        def _process_attrs(self, attrs, assign) :
            if attrs != None :
                if not self.parent.rel :
                    raise AssertionError("psect attributes only allowed in relocatable CodeBuffer")
                #end if
                for attr in attrs :
                    if attr not in self.known_attrs :
                        raise AssertionError("unrecognized psect attribute “%s”" % attr)
                    #end if
                    entry = attrs[attr]
                    if assign :
                        setattr(self, entry[0], entry[1])
                    else :
                        assert getattr(self, entry[0]) == entry[1], \
                            "inconsistent value for %s psect attribute" % entry[0]
                    #end if
                #end for
            #end if
        #end _process_attrs

        def __init__(self, name, parent, attrs) :
            self.name = name
            self.origin = None # to begin with
            self.minaddr = None
            self.maxaddr = None
            self.parent = parent
            if parent.rel :
                assert limitsym(name).decode() == name, "psect name out of symbol range"
                self.mem = self.parent.Memory()
                # defaults
                self.origin = 0
                self.data = False
                self.gbl = False
                self.rel = True
                self.overlay = False
                self.relocations = []
            else :
                self.mem = self.parent.mem
                self.rel = False
                self.relocations = None
            #end if
            self._process_attrs(attrs, True)
            parent.psects[name] = self
            parent.psects_by_index.append(self)
            self.index = len(parent.psects) # for reference in complex relocations
        #end __init__

        def setorigin(self, neworigin) :
            """updates the origin."""
            if neworigin != None :
                check_overlap = False # to begin with
                if self.minaddr == None or self.minaddr > neworigin :
                    self.minaddr = neworigin
                    check_overlap = True
                #end if
                if self.maxaddr == None or self.maxaddr < neworigin :
                    self.maxaddr = neworigin
                    check_overlap = True
                #end if
                if not self.rel and check_overlap :
                    for otherpsect in self.parent.psects.values() :
                        if (
                                otherpsect != self
                            and
                                not otherpsect.rel
                            and
                                otherpsect.minaddr != None
                            and
                                otherpsect.maxaddr != None
                            and
                                neworigin >= otherpsect.minaddr
                            and
                                neworigin <= otherpsect.maxaddr
                        ) :
                            raise AssertionError \
                              (
                                    "psect \"%s\" overlaps \"%s\" at location %#06o"
                                %
                                    (self.name, otherpsect.name, neworigin)
                              )
                        #end if
                    #end for
                #end if
            #end if
            self.origin = neworigin
            return self # for convenient chaining of calls
        #end setorigin

        def dumpw(self, start = None, end = None) :
            "generator which yields a sequence of (addr, value, reloc) triples for" \
            " nonzero or relocated word values in order of increasing address over the" \
            " specified range. start and end must be even."
            assert self.parent.rel
            raw = self.mem.dumpw(start, end)
            relocs = iter(sorted(self.relocations, key = lambda r : r.offset))
            endraw, endrelocs = False, False
            curword, curreloc = None, None
            while True :
                if not endraw and curword == None :
                    curword = next(raw, None)
                    if curword != None :
                        curaddr, curword = curword
                    else :
                        endraw = True
                    #end if
                #end if
                if not endrelocs and curreloc == None :
                    curreloc = next(relocs, None)
                    if curreloc == None :
                        endrelocs = True
                    #end if
                #end if
                if curword == None and curreloc == None :
                    break
                if curword != None and (curreloc == None or curaddr < curreloc.offset) :
                    yield curaddr, curword, None
                    curword = None
                elif curreloc != None and (curword == None or curreloc.offset < curaddr) :
                    yield curreloc.offset, 0, curreloc
                    curreloc = None
                else : # curword != None and curreloc != None and curaddr = curreloc.offset :
                    yield curaddr, curword, curreloc
                    curword, curreloc = None, None
                #end if
            #end while
        #end dumpw

    #end PsectClass

    def __init__(self, rel) :
        self.labels = {}
        self.psects = {}
        self.psects_by_index = []
        self.rel = rel
        if not rel :
            self.mem = self.Memory()
            self.psect("") # initial default psect
        #end if
        self.startaddr = None
    #end __init__

    def label(self, name, resolve_here = False, globlabel = False) :
        """defines a label with the specified name, if it doesn't already exist.
        Else returns the existing label with that name."""
        assert globlabel <= self.rel, "global labels only allowed in relocatable CodeBuffer"
        if name not in self.labels :
            self.labels[name] = self.LabelClass \
              (
                name = name,
                parent = self,
                psect = None,
                globlabel = globlabel,
                extlabel = False,
                weak = False,
              )
        else :
            assert self.labels[name].globlabel == globlabel and self.labels[name].extlabel == False, \
                "inconsistent label attributes"
        #end if
        if isinstance(resolve_here, bool) :
            if resolve_here :
                self.resolve(self.labels[name], self.curpsect.origin)
            #end if
        elif isinstance(resolve_here, (int, self.LabelClass)) :
            self.resolve(self.labels[name], resolve_here)
        #end if
        return self.labels[name]
    #end label

    def extlabel(self, name, weak = False) :
        """declares a label which is not defined in this CodeBuffer, but
        needs to be resolved in a separate linking stage."""
        if name not in self.labels :
            label = self.LabelClass \
              (
                name = name,
                parent = self,
                psect = None,
                globlabel = True,
                extlabel = True,
                weak = weak,
              )
            label.rel = True
            self.labels[name] = label
        else :
            assert self.labels[name].globlabel and self.labels[name].extlabel, \
                "inconsistent extlabel attributes"
        #end if
        return label
    #end extlabel

    def _fixup(self, label, psect, addr, reftype) :
        # common internal routine for actually fixing up a label reference.
        assert label.value != None and label.rel != None
        assert reftype in \
            (
                (self.LabelClass.b16a, self.LabelClass.b16r)
            +
                (
                    (self.LabelClass.w8, self.LabelClass.w6),
                      # NYI: probably want to be able to handle these with complex relocation
                    (),
                )[label.rel]
            )
        if reftype == self.LabelClass.b16a :
          # 16-bit absolute byte reference
            psect.mem.dw(addr, label.value)
        elif reftype == self.LabelClass.b16r :
          # 16-bit signed relative byte reference
            psect.mem.dw(addr, label.value - addr - 2)
        elif reftype == self.LabelClass.w8 :
          # 8-bit signed relative word reference (branch instr)
            assert (label.value & 1) == 0 and (addr & 1) == 0
            offset = (label.value - addr - 2) // 2
            assert offset >= -128 and offset < 128
            psect.mem.db(addr, offset & 255)
        elif reftype == self.LabelClass.w6 :
          # 6-bit negated word reference (sob instr)
            assert (label.value & 1) == 0 and (addr & 1) == 0
            offset = (addr + 2 - label.value) // 2
            assert offset >= 0 and offset < 64
            psect.mem.db(addr, psect.mem.eb(addr) & ~63 | offset)
        #end if
        if label.rel :
            Relocation = self.Relocation
            relargs = {}
            if label.extlabel :
                reltype = Relocation.RELTYPE.GLOBAL # never Relocation.RELTYPE.GLOBAL_ADDITIVE?
                relargs["name"] = label.name
            elif label.psect != psect :
                assert label.psect != None
                additive = label.value != 0
                reltype = (Relocation.RELTYPE.PSECT, Relocation.RELTYPE.PSECT_ADDITIVE)[additive]
                relargs["name"] = label.psect.name
                if additive :
                    relargs["constant"] = label.value
                #end if
            else :
                reltype = Relocation.RELTYPE.INTERNAL
            #end if
            relargs["psect"] = psect
            relargs["offset"] = addr
            relargs["reltype"] = reltype
            relargs["displaced"] = reftype == self.LabelClass.b16r
            psect.relocations.append(Relocation(**relargs))
        #end if
    #end _fixup

    def refer(self, label, addr, reftype) :
        """inserts a reference to the specified label at the specified
        location, of the specified type. May be called any number of times
        before or after the label is resolved."""
        addr = self.follow(addr)
        if label.value == None :
            label.refs.append((self.curpsect, addr, reftype)) # for later resolution/relocation
        else :
            # resolve straight away
            self._fixup(label, self.curpsect, addr, reftype)
        #end if
        return self # for convenient chaining of calls
    #end refer

    def resolve(self, label, value = None) :
        """marks the label as resolved to the specified address, or the current
        origin if None. Must be called exactly once per non-external label."""
        assert label.value == None # not already resolved
        assert not label.extlabel, "external symbol not supposed to be defined here"
        if value == None :
            label.psect = self.curpsect
            value = self.curpsect.origin
            label.rel = self.curpsect.rel
        else :
            value = self.follow(value)
            label.rel = False
        #end if
        assert value != None
        label.value = value
        for psect, addr, reftype in label.refs :
            self._fixup(label, psect, addr, reftype)
        #end for
        return self # for convenient chaining of calls
    #end resolve

    def follow(self, ref, atloc = None, reftype = None) :
        # returns ref if it's an integer, or its value if it's a resolved label.
        # An unresolved label is only allowed if atloc is not None; in which
        # case a dummy value is returned, and a reference to the label is added
        # pointing to address atloc of type reftype for fixing up later when the
        # label is resolved.
        if isinstance(ref, self.LabelClass) :
            if ref.value == None and atloc != None or ref.extlabel :
                self.refer(ref, atloc, reftype)
                ref = 0 # dummy value, filled in later
            else :
                assert ref.value != None, "reference to unresolved label %s" % ref.name
                ref = ref.value
            #end if
        else :
            ref = int(ref)
        #end if
        return ref
    #end follow

    def psect(self, name, attrs = None) :
        "sets the current program section to the one with the specified name," \
        " creating it if it doesn't already exist."
        psect = self.psects.get(name)
        if psect != None :
            psect._process_attrs(attrs, False)
        else :
            psect = self.PsectClass(name, self, attrs)
        #end if
        self.curpsect = psect
        self.mem = self.curpsect.mem
        return self # for convenient chaining of calls
    #end psect

    def eb(self, addr) :
        """returns the byte at the specified adddress."""
        return \
            self.mem.eb(self.follow(addr))
    #end eb

    def ew(self, addr) :
        """returns the word at the specified address (must be even)."""
        return \
            self.mem.ew(self.follow(addr))
    #end ew

    def db(self, addr, value) :
        """sets the byte at the specified address to the specified value."""
        self.mem.db(self.follow(addr), value)
        return self # for convenient chaining of calls
    #end db

    def dw(self, addr, value) :
        """sets the word at the specified address (must be even) to the specified value."""
        self.mem.dw(self.follow(addr), value)
        return self # for convenient chaining of calls
    #end dw

    def org(self, addr) :
        """sets the origin for defining subsequent consecutive memory contents."""
        assert addr != None
        self.curpsect.setorigin(self.follow(addr))
        return self # for convenient chaining of calls
    #end org

    def dot(self) :
        """returns the current location for inserting memory contents."""
        return self.curpsect.origin
    #end dot

    def b(self, value) :
        """inserts a byte value at the current origin and advances it."""
        assert self.curpsect.origin != None, "origin not set"
        self.db(self.curpsect.origin, value)
        self.curpsect.setorigin((self.curpsect.origin + 1) % 0x10000)
        return self # for convenient chaining of calls
    #end bi

    def w(self, value) :
        """inserts a word value at the current origin (must be even) and advances it."""
        assert self.curpsect.origin != None, "origin not set"
        assert (self.curpsect.origin & 1) == 0, "origin must be even"
        self.dw(self.curpsect.origin, value)
        self.curpsect.setorigin((self.curpsect.origin + 2) % 0x10000)
        return self # for convenient chaining of calls
    #end w

    def align(self, odd = False) :
        """ensures that the current origin is either even or odd."""
        assert self.curpsect.origin != None, "origin not set"
        if (self.curpsect.origin & 1 != 0) != odd :
            self.curpsect.origin += 1
        #end if
        return self # for convenient chaining of calls
    #end align

    def i(self, opcode, *args) :
        """inserts an instruction at the current origin (must be even) and advances it.
        Each arg can be a register operand, or a 2-tuple of register + offset, or an
        integer for an immediate-mode operand."""

        def place(val) :
          # returns a dummy integer value in place of val if it is a label.
            if isinstance(val, self.LabelClass) :
                val = 0
            # else :
                # assume it's an int
            #end if
            return val
        #end place

    #begin i
        assert len(args) == opcode.nroperands, "wrong nr operands %d, expect %d" % (len(args), opcode.nroperands)
        opnds = [] # operands for opcode
        extra = [] # extra instruction words
        refer = [] # references needing relocation--list of argument tuples for self.refer
        for arg in args :
            if isinstance(arg, o) :
                opnds.append(arg)
            elif isinstance(arg, (tuple, list)) :
                assert len(arg) == 2 and isinstance(arg[0], o) and arg[0].hasoffset, "invalid arg+offset"
                if arg[0] == o.i :
                    opnds.append(o.PCi)
                elif arg[0] == o.a :
                    opnds.append(o.aPCi)
                else :
                    opnds.append(arg[0])
                #end if
                extra.append(arg[1])
                refer.append((arg[1], self.dot() + 2 * len(refer) + 2, self.LabelClass.b16a))
            elif isinstance(arg, (int, self.LabelClass)) :
                if opcode.genmask & 1 << len(opnds) != 0 :
                    opnds.append(o.PCo) # PC-relative addressing
                    if isinstance(arg, int) :
                        extra.append(arg - self.dot() + 2 * len(refer) - 4)
                    else :
                        extra.append(arg)
                    #end if
                    refer.append((arg, self.dot() + 2 * len(refer) + 2, self.LabelClass.b16r))
                else :
                    opnds.append(place(arg))
                    assert len(refer) == 0
                    if isinstance(arg, self.LabelClass) :
                        # assume branch or SOB
                        refer.append \
                          (
                            (
                                arg,
                                self.dot(),
                                (self.LabelClass.w6, self.LabelClass.w8)[opcode != op.SOB]
                            )
                          )
                    #end if
                #end if
            else :
                raise AssertionError("invalid arg type")
            #end if
        #end for
        (instr, lenextra) = opcode(*opnds)
        assert lenextra == 2 * len(extra), "wrong number of operand offsets"
        for word in [instr] + extra :
            self.w(place(word))
        #end for
        for thisrefer in refer :
            if isinstance(thisrefer[0], self.LabelClass) :
                self.refer(*thisrefer)
            #end if
        #end for
        return self # for convenient chaining of calls
    #end i

    def start(self, startaddr) :
        """sets the start-address of the program. This is purely informational
        as far as the CodeBuffer class is concerned."""
        assert self.startaddr == None
        if self.rel :
            assert isinstance(startaddr, self.LabelClass), "relocatable start address must be a label"
            assert not startaddr.extlabel, "start address must not be external symbol"
            self.startaddr = startaddr
        else :
            self.startaddr = self.follow(startaddr)
        #end if
        return self # for convenient chaining of calls
    #end start

    def dumpb(self, start = None, end = None) :
        """generator which yields a sequence of (addr, value) pairs for nonzero byte
        values in order of increasing address over the specified range."""
        assert not self.rel
        return \
            self.mem.dumpb(start, end)
    #end dumpb

    def dumpw(self, start = None, end = None) :
        """generator which yields a sequence of (addr, value) pairs for nonzero word
        values in order of increasing address over the specified range. start and end
        must be even."""
        assert not self.rel
        return \
            self.mem.dumpw(start, end)
    #end dumpw

#end CodeBuffer

def rad50(s) :
    """returns the radix-50 encoding of s, which must be a string or a bytes
    value. The result will be a list of integers, one for each group of 3
    characters/bytes in s. If its length is not a multiple of 3, the last
    group will be padded on the end with blanks."""
    if not isinstance(s, bytes) :
        s = s.encode()
    #end if
    if len(s) % 3 != 0 :
        s += b"  "[0 : 3 - len(s) % 3]
    #end if
    result = []
    n = None
    for c in s :
        if n == None :
            n = 0
            i = 64000
        #end if
        if c >= ord(b'a') and c <= ord(b'z') :
            c = c - ord(b'a') + 1
        elif c >= ord(b'A') and c <= ord(b'Z') :
            c = c - ord(b'A') + 1
        elif c >= ord(b'0') and c <= ord(b'9') :
            c = c - ord(b'0') + 30
        elif c == ord(b' ') :
            c = 0
        elif c == ord(b'$') :
            c = 27
        elif c == ord(b'.') :
            c = 28
        else :
            c = 29
        #end if
        i //= 40
        n += c * i
        if i == 1 :
            result.append(n)
            n = None
        #end if
    #end for
    return \
        tuple(result)
#end rad50

def unrad50(n) :
    """returns the decoding of the radix-50 code(s) in n, which must be
    either an integer or a sequence of integers. Each integer is decoded
    to 3 bytes, and the result will be their concatenation in order."""
    if isinstance(n, int) :
        n = (n,)
    #end if
    result = []
    for g in n :
        i = 64000
        for j in range(0, 3) :
            i //= 40
            d = g // i
            g = g - d * i
            result.append(b" ABCDEFGHIJKLMNOPQRSTUVWXYZ$.?0123456789"[d])
        #end for
    #end for
    return \
        bytes(result)
#end unrad50

def limitsym(name) :
    """returns name after applying restrictions for an object symbol name:
    no more than 6 characters in the radix-50 character set."""
    return \
        unrad50(rad50(name))[:6].rstrip(b" ")
#end limitsym

def gen_pt_boot(buf, memsize, devaddr) :
    """generates the paper tape bootstrap loader code into the appropriate
    absolute addresses in CodeBuffer object buf. memsize is the memory size
    in integer kiB [8 .. 56], and devaddr is the device status register address
    of the paper-tape reader: 0o177560 for the teletype, 0o177550 for the
    high-speed reader."""
    assert not buf.rel, "can't handle relocatable CodeBuffer"
    base = (memsize * 1024 // 4096 - 1) << 12
    buf.org(base + 0o7744)
    buf.w(0o16701).w(0o00026) # mov xx7776, r1 ; get device status register address
    # top of read loop
    buf.w(0o12702).w(0o00352)
      #  mov #0352, r2 ; initially read byte into xx7752 = low byte of dest addr
      # low byte of source operand will be overwritten by first input byte
    buf.w(0o05211) #  inc (r1)    ; await another char
    buf.w(0o105711) #  tstb (r1)   ; read done?
    buf.w(0o100376) #  bpl xx7756  ; keep testing if not
    buf.w(0o116162).w(0o000002).w(base + 0o7400)
      #  movb 2(r1), xx7400(r2) ; read byte into dest addr
    buf.w(0o005267).w(0o177756) #  inc xx7752  ; step dest addr
    buf.w(0o000765) #  br xx7750 ; back to top of read loop
    buf.w(devaddr)
#end gen_pt_boot

def dump_simh(buf, out) :
    """dumps the contents of CodeBuffer object buf to out as a sequence of
    SIMH memory-deposit commands."""
    assert not buf.rel, "can't handle relocatable CodeBuffer"
    for addr, value in buf.dumpw(0, 0x10000) :
        out.write("d %4o %4o\n" % (addr, value))
    #end for
    if buf.startaddr != None :
        out.write("g %o\n" % buf.startaddr)
    #end if
#end dump_simh

@enum.unique
class OBJFORMAT(enum.Enum) :
    "formats for object files."
    FB = 1
      # “formatted-binary”; RSTS/E PIP can read these from, e.g. paper tape
      # and convert them to RSX variable-length records with /RMS:FB output
      # qualifier.
    VLR = 2
      # RSX variable-length records. May not be properly readable unless
      # you can put the right RMS attributes on the file.
#end OBJFORMAT

abs_psect_name = ". ABS."

def write_obj(format, buf, outname, modulename = None, vername = None) :
    "writes the contents of CodeBuffer to the file named outname" \
    " as an .OBJ file in either formatted-binary or variable-length-record" \
    " formats, depending on format, which must be an OBJFORMAT.xxx value."

    Relocation = CodeBuffer.Relocation
    ComplexOp = Relocation.ComplexOp
    out = None
    curgsd = []
    currld = []

    def encodesym(name) :
        # returns a tuple of 2 integers representing the radix-50 encoded
        # representation of name (truncated or blank-padded to 6 bytes
        # as appropriate).
        # FIXME: should report errors for names that collide after
        # case conversion and length truncation
        return \
            tuple(rad50(name.encode() + b"     "[:6 - len(name)]))[:2]
    #end encodesym

    def wrrec_fb(data) :
        "writes data as formatted-binary record."
        # is there any limit to the length of data?
        encoded = struct.pack("<HH", 1, len(data) + 4) + data
        checksum = sum(-b for b in encoded)
        out.write(encoded)
        out.write(bytes((checksum & 255,)))
    #end wrrec_fb

    def wrrec_vlr(data) :
        "writes data as a variable-length record."
        assert len(data) > 0 and len(data) <= 128, "object record mustn’t exceed 128 bytes"
        out.write(struct.pack("<H", len(data) + 2) + data + bytes((0,))[0:len(data) % 2])
    #end wrrec_vlr

    wrrec = {OBJFORMAT.FB : wrrec_fb, OBJFORMAT.VLR : wrrec_vlr}[format]

    def flushgsd() :
        if len(curgsd) != 0 :
            data = struct.pack("<H", 1)
            for entry in curgsd :
                data += struct.pack("<HHBBH", *entry)
            #end for
            curgsd[:] = []
            wrrec(data)
        #end if
    #end flushgsd

    def addgsd(name, type, flags, value) :
        if len(curgsd) == 15 :
            flushgsd()
        #end if
        curgsd.append \
            (
                encodesym(name) + (flags, type, value),
            )
    #end addgsd

    def endgsd() :
        flushgsd()
        wrrec(struct.pack("<H", 2)) # end of GSD
    #end endgsd

    def flushrld() :
        if len(currld) != 0 :
            wrrec(struct.pack("<H" + "H" * len(currld), *([4] + currld)))
            currld[:] = []
        #end if
    #end flushrld

    def addrld(entry) :
        assert len(entry) < 64
        if len(currld) + len(entry) >= 64 :
            flushrld()
        #end if
        currld.extend(entry)
    #end addrld

#begin write_obj
    out = open(outname, "wb")
    if modulename != None :
        addgsd(modulename, 0, 0, 0)
    #end if
    if vername != None :
        addgsd(vername, 6, 0, 0)
    #end if
    if buf.rel :
        for label in buf.labels.values() :
            if label.extlabel :
                addgsd \
                  (
                    name = label.name,
                    type = 4,
                    flags = int(label.weak) << 0,
                    value = 0
                  )
            #end if
        #end for
        for psect in buf.psects_by_index :
            if psect.maxaddr != None :
                flags = 0
                for \
                    attr, flag \
                in \
                    ( \
                        ("data", 7),
                        ("gbl", 6),
                        ("rel", 5),
                        ("overlay", 2),
                    ) \
                :
                    if getattr(psect, attr) :
                        flags |= 1 << flag
                    #end if
                #end for
                addgsd(psect.name, 5, flags, psect.maxaddr)
                for label in buf.labels.values() :
                    if label.globlabel and label.psect == psect :
                        addgsd \
                          (
                            name = label.name,
                            type = 4,
                            flags =
                                    int(label.weak) << 0
                                |
                                    1 << 3
                                |
                                    int(psect.rel) << 5,
                            value = label.value
                          )
                    #end if
                #end for
                psect_reloc = None
            #end if
        #end for
        if buf.startaddr != None :
            addgsd(buf.startaddr.psect.name, 3, 0, buf.startaddr.value)
        #end if
    else :
        addgsd(abs_psect_name, 5, 0, 0)
        if buf.startaddr != None :
            addgsd(abs_psect_name, 3, 0, buf.startaddr)
        #end if
    #end if
    endgsd()
    last_psect_name = None
    for psect in ((buf.curpsect,), buf.psects_by_index)[buf.rel] :
        psect_name = (abs_psect_name, psect.name)[buf.rel]
        if buf.rel :
            dumper = psect.dumpw()
        else :
            dumper = psect.mem.dumpw()
        #end if
        nextaddr = None
        startaddr = None
        lasttxt = []
        while True :
            value = next(dumper, None)
            if value != None :
                if buf.rel :
                    addr, value, reloc = value
                else :
                    addr, value = value
                    reloc = None
                #end if
            else :
                addr, reloc = None, None
            #end if
            if addr != None and psect_name != last_psect_name :
                addrld((7,) + encodesym(psect_name) + (addr,))
                flushrld()
                last_psect_name = psect_name
                nextaddr = addr
                startaddr = addr
            #end if
            if addr == None or addr != nextaddr or len(lasttxt) == 63 :
                if len(lasttxt) != 0 :
                    wrrec(struct.pack("<HH" + "H" * len(lasttxt), *([3, startaddr] + lasttxt)))
                      # text record
                #end if
                if addr != None and nextaddr != addr :
                  # output location counter definition for following text record(s)
                    addrld((8, addr))
                #end if
                flushrld()
                startaddr = addr
                lasttxt = []
            #end if
            if addr == None :
                break
            lasttxt.append(value)
            nextaddr = addr + 2
            if reloc != None :
                if reloc.reltype == Relocation.RELTYPE.INTERNAL :
                    rldentry = ((1, 3)[reloc.displaced], reloc.constant)
                elif reloc.reltype == Relocation.RELTYPE.GLOBAL :
                    rldentry = ((2, 4)[reloc.displaced],) + encodesym(reloc.name)
                elif reloc.reltype == Relocation.RELTYPE.GLOBAL_ADDITIVE :
                    rldentry = ((5, 6)[reloc.displaced],) + encodesym(reloc.name) + (reloc.constant,)
                elif reloc.reltype == Relocation.RELTYPE.PSECT :
                    rldentry = ((10, 12)[reloc.displaced],) + encodesym(reloc.name)
                elif reloc.reltype == Relocation.RELTYPE.PSECT_ADDITIVE :
                    rldentry = ((13, 14)[reloc.displaced],) + encodesym(reloc.name) + (reloc.constant,)
                elif reloc.reltype == Relocation.RELTYPE.COMPLEX :
                    complops = []
                      # collect contents of complex relocation entry
                      # list of tuples, each (value, isword)
                    for operand in reloc.operands :
                        complops.append((operand.op.value, False))
                        if operand.op == ComplexOp.OP.FETCH_GLOBAL :
                            complops.append((n, True) for n in encodesym(operand.name))
                        elif operand.op == ComplexOp.OP.FETCH_REL :
                            complops.extend \
                              (
                                [(buf.psects[operand.name].index, False), (operand.constant, True)]
                              )
                        elif operand.op == ComplexOp.OP.FETCH_CONST :
                            complops.append((operand.constant, True))
                        else : # assume no further info for op
                            pass
                        #end if
                    #end for
                    complops.append \
                      (
                        (
                            (ComplexOp.OP.STORE, ComplexOp.OP.STORE_DISP)[reloc.displaced].value,
                            False
                        )
                      )
                    # turn complex relocation entry into sequence of words
                    # TBD no -- doesn’t have to be word-aligned!
                    rldrest = []
                    complops = iter(complops)
                    partword = None
                    while True :
                        item = next(complops, None)
                        if item == None :
                            if partword != None :
                                rldrest.append(partword) # high byte is zero
                            #end if
                            break
                        #end if
                        item, isword = item
                        if isword :
                            if partword != None :
                                rldrest.append(partword | (item & 255) << 8)
                                partword = item >> 8 & 255
                            else :
                                rldrest.append(item)
                                # and partword stays None
                            #end if
                        else :
                            if partword != None :
                                rldrest.append(partword | item << 8)
                                partword = None
                            else :
                                partword = item
                            #end if
                        #end if
                    #end while
                    rldentry = (15,) + tuple(rldrest)
                else :
                    raise AssertionError("unrecognized relocation type %s" % repr(reloc.reltype))
                #end if
                addrld((rldentry[0] | addr - startaddr + 4 << 8,) + rldentry[1:])
            #end if reloc != None
        #end while
    #end for
    flushrld()
    wrrec(struct.pack("<H", 6)) # end of module
    out.flush()
    out.close()
#end write_obj
