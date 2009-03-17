#+
# A machine-code generation library for the PDP-11.
#
# Written by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#-

class CodeBuffer(object) :
	"""overall management of a block of generated code."""

	def __init__(self) :
		self.blocks = [[] * 8] # contiguous sequences of defined memory contents
		  # length must exactly divide 65536
		self.baseaddrs = [None] * len(self.blocks)
		self.origin = None
		self.labels = {}
		self.startaddr = None
	#end __init__

	def eb(self, addr) :
		"""returns the byte at the specified adddress."""
		(index, offset) = divmod(addr, (65536 / len(self.blocks)))
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
		assert (addr & 1) == 0
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
		assert (addr & 1) == 0
		return self.db(addr, value & 255).db(addr + 1, value >> 8 & 255)
	#end dw

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
		values in order of increasing address over the specified range."""
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
