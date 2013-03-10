#+
# A machine-code generation library for the PDP-11.
#
# Written by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#-

class o(object) :
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

class cc(object) :
    """condition codes flags bits."""
    C = 1
    V = 2
    Z = 4
    N = 8
#end cc

class op(object) :
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
        assert type(opnd) == int, "branch offset must be integer"
        assert opnd >= -128 and opnd < 128, "branch offset outside valid range"
        return (bitpat | (opnd & 255), 0)
    #end branchonly

    @staticmethod
    def regandbranch(bitpat, opnd1, opnd2) :
        # operands for SOB instruction
        operand1 = op.regonly(opnd1, 6)
        assert type(opnd2) == int, "operand2 must be integer"
        assert opnd2 >= 0 and opnd2 < 64, "branch offset outside valid range"
        return (bitpat | operand1[0] | (opnd & 63), operand1[1])
    #end regandbranch

    @staticmethod
    def byteoperand(bitpat, opnd) :
        # literal byte operand for EMT and TRAP instructions
        assert type(opnd) == int, "operand must be integer"
        assert opnd >= 0 and opnd < 256, "byte operand outside valid range"
        return (bitpat | (opnd & 255), 0)
    #end byteoperand

    @staticmethod
    def priooperand(bitpat, opnd) :
        # literal priority for SPL instruction
        assert type(opnd) == int, "operand must be integer"
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
        assert type(opnd) == int, "operand must be integer"
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
            assert type(opnd) == int, "operand must be integer"
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

class CodeBuffer(object) :
    """overall management of a block of generated code."""

    class LabelClass(object) :
        """representation of a label within the CodeBuffer."""

        # types of label references:
        b16a = 0 # 16-bit absolute byte reference
        b16r = 1 # 16-bit signed relative byte reference
        w8 = 2 # 8-bit signed relative word reference (branch instr)
        w6 = 3 # 6-bit negated word reference (sob instr)

        def __init__(self, name, parent) :
            self.refs = []
            self.name = name
            self.value = None # to begin with
            self.parent = parent
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
            if type(reg) == o :
                assert reg.hasoffset, "offset not allowed on this operand"
                return (reg, self)
            else :
                return NotImplemented
            #end if
        #end __add__

        __radd__ = __add__

        # note no __sub__ or __rsub__

    #end LabelClass

    class PsectClass(object) :
        """representation of a program section within the CodeBuffer. Besides
        allowing logical grouping of code and data sections, I also provide
        automatic checking that psects don't run into each other."""

        def __init__(self, name, parent) :
            self.name = name
            self.origin = None # to begin with
            self.minaddr = None
            self.maxaddr = None
            self.parent = parent
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
                if check_overlap :
                    for otherpsect in self.parent.psects.values() :
                        if (
                                otherpsect != self
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

    #end PsectClass

    def __init__(self) :
        self.blocks = [[] * 8] # contiguous sequences of defined memory content bytes
          # length must exactly divide 65536
        self.baseaddrs = [None] * len(self.blocks)
          # address corresponding to start of each block
        self.labels = {}
        self.psects = {}
        self.psect("") # initial default psect
        self.startaddr = None
    #end __init__

    def label(self, name, resolve_here = False) :
        """defines a label with the specified name, if it doesn't already exist.
        Else returns the existing label with that name."""
        if name not in self.labels :
            self.labels[name] = self.LabelClass(name, self)
        #end if
        if type(resolve_here) in (int, self.LabelClass) :
            self.resolve(self.labels[name], resolve_here)
        elif resolve_here :
            self.resolve(self.labels[name], self.curpsect.origin)
        #end if
        return self.labels[name]
    #end label

    def _fixup(self, label, addr, reftype) :
        # common internal routine for actually fixing up a label reference.
        assert label.value != None
        if reftype == self.LabelClass.b16a :
          # 16-bit absolute byte reference
            self.dw(addr, label.value)
        elif reftype == self.LabelClass.b16r :
          # 16-bit signed relative byte reference
            self.dw(addr, label.value - addr - 2)
        elif reftype == self.LabelClass.w8 :
          # 8-bit signed relative word reference (branch instr)
            assert (label.value & 1) == 0 and (addr & 1) == 0
            offset = (label.value - addr - 2) // 2
            assert offset >= -128 and offset < 128
            self.db(addr, offset & 255)
        elif reftype == self.LabelClass.w6 :
          # 6-bit negated word reference (sob instr)
            assert (label.value & 1) == 0 and (addr & 1) == 0
            offset = (addr + 2 - label.value) // 2
            assert offset >= 0 and offset < 64
            self.db(addr, self.eb(addr) & ~63 | offset)
        #end if
    #end _fixup

    def refer(self, label, addr, reftype) :
        """inserts a reference to the specified label at the specified
        location, of the specified type. May be called any number of times
        before or after the label is resolved."""
        addr = self.follow(addr)
        assert reftype in \
            (self.LabelClass.b16a, self.LabelClass.b16r, self.LabelClass.w8, self.LabelClass.w6)
        if label.value != None :
            # resolve straight away
            self._fixup(label, addr, reftype)
        else :
            label.refs.append((addr, reftype)) # for later resolution
        #end if
        return self # for convenient chaining of calls
    #end refer

    def resolve(self, label, value = None) :
        """marks the label as resolved to the specified address, or the current
        origin if None. Must be called exactly once per label."""
        assert label.value == None # not already resolved
        if value == None :
            value = self.curpsect.origin
        else :
            value = self.follow(value)
        #end if
        assert value != None
        label.value = value
        for addr, reftype in label.refs :
            self._fixup(label, addr, reftype)
        #end for
        label.refs = []
        return self # for convenient chaining of calls
    #end resolve

    def follow(self, ref, atloc = None, reftype = None) :
        # returns ref if it's an integer, or its value if it's a resolved label.
        # An unresolved label is only allowed if atloc is not None; in which
        # case a dummy value is returned, and a reference to the label is added
        # pointing to address atloc of type reftype for fixing up later when the
        # label is resolved.
        if type(ref) == self.LabelClass :
            if ref.value == None and atloc != None :
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

    def psect(self, name) :
        """sets the current program section to the one with the specified name,
        creating it if it doesn't already exist."""
        if name not in self.psects :
            self.psects[name] = self.PsectClass(name, self)
        #end if
        self.curpsect = self.psects[name]
        return self # for convenient chaining of calls
    #end psect

    def eb(self, addr) :
        """returns the byte at the specified adddress."""
        addr = self.follow(addr)
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
        return result
    #end eb

    def ew(self, addr) :
        """returns the word at the specified address (must be even)."""
        addr = self.follow(addr)
        assert (addr & 1) == 0, "address must be even"
        return self.eb(addr) | self.eb(addr + 1) << 8
    #end ew

    def db(self, addr, value) :
        """sets the byte at the specified address to the specified value."""
        addr = self.follow(addr)
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
        addr = self.follow(addr)
        assert (addr & 1) == 0, "address must be even"
        return self.db(addr, value & 255).db(addr + 1, value >> 8 & 255)
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
            if type(val) == self.LabelClass :
                val = 0
            # else :
                # assume it's an int
            #end if
            return val
        #end place

    #begin i
        assert len(args) == opcode.nroperands, "wrong nr operands %d, expect %d" % (len(args), opcode.nroperands)
        opnds = []
        extra = []
        refer = []
        for arg in args :
            if type(arg) == o :
                opnds.append(arg)
            elif type(arg) in (tuple, list) :
                assert len(arg) == 2 and type(arg[0]) == o and arg[0].hasoffset, "invalid arg+offset"
                if arg[0] == o.i :
                    opnds.append(o.PCi)
                elif arg[0] == o.a :
                    opnds.append(o.aPCi)
                else :
                    opnds.append(arg[0])
                #end if
                extra.append(arg[1])
                refer.append((arg[1], self.dot() + 2 * len(refer) + 2, self.LabelClass.b16a))
            elif type(arg) in (int, self.LabelClass) :
                if (opcode.genmask & 1 << len(opnds)) != 0 :
                    opnds.append(o.PCo) # PC-relative addressing
                    if type(arg) == int :
                        extra.append(arg - self.dot() + 2 * len(refer) - 4)
                    else :
                        extra.append(arg)
                    #end if
                    refer.append((arg, self.dot() + 2 * len(refer) + 2, self.LabelClass.b16r))
                else :
                    opnds.append(place(arg))
                    assert len(refer) == 0
                    if type(arg) == self.LabelClass :
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
            if type(thisrefer[0]) == self.LabelClass :
                self.refer(*thisrefer)
            #end if
        #end for
        return self # for convenient chaining of calls
    #end i

    def start(self, startaddr) :
        """sets the start-address of the program. This is purely informational
        as far as ths CodeBuffer class is concerned."""
        assert self.startaddr == None
        self.startaddr = self.follow(startaddr)
        return self # for convenient chaining of calls
    #end start

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
        raise StopIteration
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
                raise StopIteration
            #end if
        #end while
    #end dumpw

#end CodeBuffer

def dump_simh(buf, out) :
    """dumps the contents of CodeBuffer object buf to out as a sequence of
    SIMH memory-deposit commands."""
    for addr, value in buf.dumpw(0, 0x10000) :
        out.write("d %4o %4o\n" % (addr, value))
    #end for
    if buf.startaddr != None :
        out.write("g %o\n" % buf.startaddr)
    #end if
#end dump_simh
