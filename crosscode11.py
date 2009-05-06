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
		return (self, offset)
	#end __add__

	def __init__(self, reg, ind, postinc, predec, hasoffset, bitpat) :
		self.reg = reg # register nr [0 .. 7]
		self.ind = ind # indirect (deferred)
		self.postinc = postinc
		self.predec = predec
		self.hasoffset = hasoffset
		self.bitpat = bitpat
	#end __init__

	# Valid operand forms are defined as publicly-visible attributes of
	# this class. Offset fields NYI

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
	setattr(o, "a" + name, o(reg, True, False, False, False, 010 | reg))
	setattr(o, name + "i", o(reg, False, True, False, not doindir, 020 | reg))
	setattr(o, "a" + name + "i", o(reg, True, True, False, not doindir, 030 | reg))
	if doindir :
		# PC auto-decrement not supported
		setattr(o, "p" + name, o(reg, False, False, True, False, 040 | reg))
		setattr(o, "ap" + name, o(reg, True, False, True, False, 050 | reg))
	#end if
	setattr(o, name + "o", o(reg, False, False, False, True, 060 | reg))
	setattr(o, "a" + name + "o", o(reg, True, False, False, True, 070 | reg))
#end for
setattr(o, "a", o(7, True, True, False, True, 037)) # for absolute addressing
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

	# internal instruction-construction and operand-validation utilities

	@staticmethod
	def regonly(opnd, offset) :
		"""validates a register-only operand."""
		assert (opnd.bitpat & 070) == 0, "register operand only"
		return (opnd.reg << offset, 0)
	#end regonly

	@staticmethod
	def general(opnd, offset) :
		"""validates a general operand."""
		return (opnd.bitpat << offset, (0, 2)[opnd.hasoffset])
	#end general

	@staticmethod
	def branchonly(bitpat, opnd) :
		assert type(opnd) == int, "branch offset must be integer for now"
		assert opnd >= -128 and opnd < 128, "branch offset outside valid range"
		return (bitpat | (opnd & 255), 0)
	#end branchonly

	@staticmethod
	def regandbranch(bitpat, opnd1, opnd2) :
		operand1 = op.regonly(opnd1, 6)
		assert type(opnd2) == int, "operand2 must be integer for now"
		assert opnd2 >= 0 and opnd2 < 64, "branch offset outside valid range"
		return (bitpat | operand1[0] | (opnd & 63), operand1[1])
	#end regandbranch

	@staticmethod
	def byteoperand(bitpat, opnd) :
		assert type(opnd) == int, "operand must be integer"
		assert opnd >= 0 and opnd < 256, "branch offset outside valid range"
		return (bitpat | (opnd & 255), 0)
	#end byteoperand

	@staticmethod
	def priooperand(bitpat, opnd) :
		assert type(opnd) == int, "operand must be integer"
		assert opnd >= 0 and opnd < 8, "priority outside valid range"
		return (bitpat | (opnd & 7), 0)
	#end byteoperand

	@staticmethod
	def regoperand(bitpat, opnd) :
		operand = op.regonly(opnd, 0)
		return (bitpat | operand[0], operand[1])
	#end regoperand

	@staticmethod
	def markoperand(bitpat, opnd) :
		assert type(opnd) == int, "operand must be integer"
		assert opnd >= 0 and opnd < 64, "mark count outside valid range"
		return (bitpat | (opnd & 63), 0)
	#end markoperand

	@staticmethod
	def nooperand(bitpat) :
		return (bitpat, 0)
	#end nooperand

	@staticmethod
	def singleoperand(bitpat, opnd) :
		operand = op.general(opnd, 0)
		return (bitpat | operand[0], operand[1])
	#end singleoperand

	@staticmethod
	def singleregoperand(bitpat, opnd) :
		operand = op.regonly(opnd, 0)
		return (bitpat | operand[0], operand[1])
	#end singleregoperand

	@staticmethod
	def condoperand(bitpat, opnd = None) :
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
		operand1 = op.general(opnd1, 6)
		operand2 = op.general(opnd2, 0)
		return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
	#end doubleoperand

	@staticmethod
	def opandreg(bitpat, opnd1, opnd2) :
		operand1 = op.general(opnd1, 0)
		operand2 = op.regonly(opnd2, 6)
		return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
	#end opandreg

	@staticmethod
	def regandop(bitpat, opnd1, opnd2) :
		operand1 = op.regonly(opnd1, 6)
		operand2 = op.general(opnd2, 0)
		return (bitpat | operand1[0] | operand2[0], operand1[1] + operand2[1])
	#end regandop

	def __init__(self, bitpat, nroperands, genmask, instr) :
		self.bitpat = bitpat
		self.nroperands = nroperands
		self.genmask = genmask
		self.instr = instr
	#end __init__

	def __call__(self, *operands) :
		return self.instr(*(self.bitpat,) + operands)
	#end __call__

	# All the valid instructions are defined as publicly-visible attributes
	# of this class. The value of each is a function that takes instruction
	# operands as arguments, and returns a 2-tuple, with item 0 being the
	# value of the first (or only) instruction word, and item 1 being the
	# number of subsequent operand words.

#end op

for \
		name, bitpat \
	in \
		(
			("CLR", 0005000),
			("COM", 0005100),
			("INC", 0005200),
			("DEC", 0005300),
			("NEG", 0005400),
			("ADC", 0005500),
			("SBC", 0005600),
			("TST", 0005700),
			("ROR", 0006000),
			("ROL", 0006100),
			("ASR", 0006200),
			("ASL", 0006300),
		) \
:
	setattr(op, name, op(bitpat, 1, 1, op.singleoperand))
	setattr(op, name + "B", op(bitpat | 0100000, 1, 1, op.singleoperand))
#end for
setattr(op, "SWAB", op(000300, 1, 1, op.singleoperand))
setattr(op, "SXT", op(0006700, 1, 1, op.singleoperand))
for \
		name, bitpat \
	in \
		(
			("MOV", 010000),
			("CMP", 020000),
			("CMP", 020000),
			("BIT", 030000),
			("BIC", 040000),
			("BIS", 050000),
		) \
:
	setattr(op, name, op(bitpat, 2, 3, op.doubleoperand))
	setattr(op, name + "B", op(bitpat | 0100000, 2, 3, op.doubleoperand))
#end for
setattr(op, "ADD", op(0060000, 2, 3, op.doubleoperand))
setattr(op, "SUB", op(0160000, 2, 3, op.doubleoperand))
for \
		name, bitpat \
	in \
		(
			("MUL", 070000),
			("DIV", 071000),
			("ASH", 072000),
			("ASHC", 073000),
		) \
:
	setattr(op, name, op(bitpat, 2, 3, op.opandreg))
#end for
setattr(op, "XOR", op(074000, 2, 3, op.regandop))
for \
		name, bitpat \
	in \
		(
			("BR", 0000400),
			("BNE", 0001000),
			("BEQ", 0001400),
			("BPL", 0100000),
			("BMI", 0100400),
			("BVC", 0102000),
			("BVS", 0102400),
			("BCC", 0103000),
			("BCS", 0103400),
			("BGE", 0002000),
			("BLT", 0002400),
			("BGT", 003000),
			("BLE", 003400),
			("BHI", 0101000),
			("BLOS", 0101400),
			("BHIS", 0103000),
			("BLO", 0103400),
		) \
:
	setattr(op, name, op(bitpat, 1, 0, op.branchonly))
#end for
setattr(op, "JMP", op(000100, 1, 1, op.singleoperand))
setattr(op, "JSR", op(004000, 2, 3, op.regandop))
setattr(op, "RTS", op(000200, 1, 1, op.singleregoperand))
setattr(op, "MARK", op(006400, 1, 0, op.markoperand))
setattr(op, "SOB", op(077000, 2, 1, op.regandbranch))
setattr(op, "EMT", op(0104000, 1, 0, op.byteoperand))
setattr(op, "TRAP", op(0104400, 1, 0, op.byteoperand))
setattr(op, "BPT", op(000003, 0, 0, op.nooperand))
setattr(op, "IOT", op(000004, 0, 0, op.nooperand))
setattr(op, "RTI", op(000002, 0, 0, op.nooperand))
setattr(op, "RTT", op(000006, 0, 0, op.nooperand))
setattr(op, "SPL", op(000230, 1, 0, op.priooperand))
setattr(op, "HALT", op(000000, 0, 0, op.nooperand))
setattr(op, "WAIT", op(000001, 0, 0, op.nooperand))
setattr(op, "RESET", op(000005, 0, 0, op.nooperand))
for \
		name, bitpat \
	in \
		(
			("MFPI", 0006500),
			("MTPI", 0006600),
			("MFPD", 0106500),
			("MTPD", 0106600),
		) \
:
	setattr(op, name, op(bitpat, 1, 1, op.singleoperand))
#end for
for name in dir(cc) :
	if name[0] != "_" :
		setattr(op, "CL" + name, op(000240 | getattr(cc, name), 0, 0, op.nooperand))
		setattr(op, "SE" + name, op(000260 | getattr(cc, name), 0, 0, op.nooperand))
	#end if
#end for
setattr(op, "CCC", op(000240, 1, 0, op.condoperand))
setattr(op, "SCC", op(000260, 1, 0, op.condoperand))
setattr(op, "NOP", op(000240, 1, 0, op.nooperand))
# FIS:
setattr(op, "FADD", op(0750000, 1, 1, op.regoperand))
setattr(op, "FSUB", op(0750010, 1, 1, op.regoperand))
setattr(op, "FMUL", op(0750020, 1, 1, op.regoperand))
setattr(op, "FDIV", op(0750030, 1, 1, op.regoperand))
del name, bitpat
# FPP, CIS NYI

class CodeBuffer(object) :
	"""overall management of a block of generated code."""

	def __init__(self) :

		class register(object) :

			def __init__(self, nr) :
				self.nr = nr
			#end __init__

		#end register

		class op(object) :
			pass # TBD
		#end op

	#begin __init__
		self.blocks = [[] * 8] # contiguous sequences of defined memory content bytes
		  # length must exactly divide 65536
		self.baseaddrs = [None] * len(self.blocks)
		  # address corresponding to start of each block
		self.origin = None
		self.labels = {}
		self.startaddr = None
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
		return result
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

	def org(self, addr) :
		"""sets the origin for defining subsequent consecutive memory contents."""
		assert addr != None
		self.origin = addr
		return self # for convenient chaining of calls
	#end org

	def b(self, value) :
		"""inserts a byte value at the current origin and advances it."""
		assert self.origin != None, "origin not set"
		self.db(self.origin, value)
		self.origin = (self.origin + 1) % 0x10000
		return self # for convenient chaining of calls
	#end bi

	def w(self, value) :
		"""inserts a word value at the current origin (must be even) and advances it."""
		assert self.origin != None, "origin not set"
		assert (self.origin & 1) == 0, "origin must be even"
		self.dw(self.origin, value)
		self.origin = (self.origin + 2) % 0x10000
		return self # for convenient chaining of calls
	#end w

	def i(self, opcode, *args) :
		"""inserts an instruction at the current origin (must be even) and advances it.
		Each arg can be a register operand, or a 2-tuple of register + offset, or an
		integer for an immediate-mode operand."""
		assert len(args) == opcode.nroperands, "wrong nr operands"
		opnds = []
		extra = []
		for arg in args :
			if type(arg) == o :
				opnds.append(arg)
			elif type(arg) in (tuple, list) :
				assert len(arg) == 2 and type(arg[0] == o) and arg[0].hasoffset, "invalid arg+offset"
				opnds.append(arg[0])
				extra.append(arg[1])
			elif type(arg) == int :
				if (opcode.genmask & 1 << len(opnds)) != 0 :
					opnds.append(o.PCi)
					extra.append(arg)
				else :
					opnds.append(arg)
				#end if
			else :
				raise AssertionError("invalid arg type")
			#end if
		#end for
		(instr, lenextra) = opcode(*opnds)
		assert lenextra == 2 * len(extra), "wrong number of operand offsets"
		for word in [instr] + extra :
			self.w(word)
		#end for
		return self # for convenient chaining of calls
	#end i

	def start(self, startaddr) :
		"""sets the start-address of the program. This is purely informational
		as far as ths CodeBuffer class is concerned."""
		assert self.startaddr == None
		self.startaddr = startaddr
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
				(addr, value) = dump.next()
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
