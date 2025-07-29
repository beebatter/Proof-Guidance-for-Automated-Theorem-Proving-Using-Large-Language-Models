from itertools import *

# Ordering
class Ord:
	gt = 1
	lt = -1
	eq = 0
	inc = 42

	# Reverse ordering
	def rev(x):
		if x == Ord.eq or x == Ord.inc:
			return x
		elif x == Ord.gt:
			return Ord.lt
		elif x == Ord.lt:
			return Ord.gt
		else:
			raise ValueError

	def to_sym(x):
		if x == Ord.eq:
			return '='
		elif x == Ord.gt:
			return '≻'
		elif x == Ord.lt:
			return '≺'
		elif x == Ord.inc:
			return '?'
		else:
			raise ValueError

	def to_string(x):
		if x == Ord.eq:
			return 'eq'
		elif x == Ord.gt:
			return 'gt'
		elif x == Ord.lt:
			return 'lt'
		elif x == Ord.inc:
			return 'inc'
		else:
			raise ValueError

	def to_int(x):
		if x == Ord.eq:
			return 0
		elif x == Ord.gt:
			return 1
		elif x == Ord.lt:
			return 2
		elif x == Ord.inc:
			return 3
		else:
			raise ValueError


# Multiset representing positive equality
def pos_eq(a,b):
	return [a,b]

# Multiset representing negative equality
def neg_eq(a,b):
	return [a,a,b,b]

def make_eq(sign,a,b):
	if sign:
		return pos_eq(a,b)
	else:
		return neg_eq(a,b)

# s=t 
# u=v
s, t, u, v = range(4)

class Multiset:
	# Multiset difference (a-b under equality in cmp)
	def diff(a, b, cmp):
		# print(a, '-', b)
		r = a[:]
		for i in b:
			for j in r:
				if cmp[(i,j)] == Ord.eq:
					r.remove(j)
					break
		# print('=', r)
		return r

	# a > b, in multiset extension of cmp
	def gt(a, b, cmp):
		ab = Multiset.diff(a,b,cmp)
		ba = Multiset.diff(b,a,cmp)
		if ab != [] and all(any(cmp[(x,y)] == Ord.gt for x in ab) for y in ba):
		# if ab != [] and all(any(x>y for x in ab) for y in ba):
			# print('>')
			return True
		else:
			# print('!>')
			return False

	# a = b, in multiset extension of cmp
	def eq(a, b, cmp):
		if a == b:
			return True
		elif len(a) == len(b):
			for i,j in zip(a,b):
				if cmp[(i,j)] != Ord.eq:
					return False
			return True
		else:
			return False

	# Return Ord, in multiset extension of cmp
	def ord(a, b, cmp):
		if Multiset.eq(a, b, cmp):
			return Ord.eq
		elif Multiset.gt(a, b, cmp):
			return Ord.gt
		elif Multiset.gt(b, a, cmp):
			return Ord.lt
		else:
			return Ord.inc



# Checks if ordering is transitive
def transitive(s, cmp):
	for x,y,z in product(s, repeat=3):
		if cmp[(x,y)] == Ord.gt and cmp[(y,z)] in [Ord.gt,Ord.eq] and cmp[(x,z)] != Ord.gt \
		or cmp[(x,y)] in [Ord.gt,Ord.eq] and cmp[(y,z)] == Ord.gt and cmp[(x,z)] != Ord.gt:
			# print(x, y, z, 
			# 	Ord.to_sym(cmp[(x,y)]),
			# 	Ord.to_sym(cmp[(y,z)]),
			# 	Ord.to_sym(cmp[(x,z)]),
			# )
			return False
		if cmp[(x,y)] == Ord.lt and cmp[(y,z)] in [Ord.lt,Ord.eq] and cmp[(x,z)] != Ord.lt \
		or cmp[(x,y)] in [Ord.lt,Ord.eq] and cmp[(y,z)] == Ord.lt and cmp[(x,z)] != Ord.lt:
			# print(x, y, z, 
			# 	Ord.to_sym(cmp[(x,y)]),
			# 	Ord.to_sym(cmp[(y,z)]),
			# 	Ord.to_sym(cmp[(x,z)]),
			# )
			return False
		if cmp[(x,y)] == Ord.eq and cmp[(y,z)] == Ord.eq and cmp[(x,z)] != Ord.eq:
			# print(x, y, z, 
			# 	Ord.to_sym(cmp[(x,y)]),
			# 	Ord.to_sym(cmp[(y,z)]),
			# 	Ord.to_sym(cmp[(x,z)]),
			# )
			return False
	return True






results = {}

# print('S1 S2 st uv su sv tu tv | res')
print('(*      S1  S2  st   uv   su   sv   tu   tv |  res *)')
for sign1, sign2 in product([True,False], repeat=2): 
	for st, uv, su, sv, tu, tv in product([Ord.gt,Ord.lt,Ord.eq,Ord.inc], repeat=6):
		orderings = {}
		orderings[(s,t)] = st
		orderings[(t,s)] = Ord.rev(st)
		orderings[(s,u)] = su
		orderings[(u,s)] = Ord.rev(su)
		orderings[(s,v)] = sv
		orderings[(v,s)] = Ord.rev(sv)
		orderings[(t,u)] = tu
		orderings[(u,t)] = Ord.rev(tu)
		orderings[(t,v)] = tv
		orderings[(v,t)] = Ord.rev(tv)
		orderings[(u,v)] = uv
		orderings[(v,u)] = Ord.rev(uv)
		orderings[(s,s)] = Ord.eq
		orderings[(t,t)] = Ord.eq
		orderings[(u,u)] = Ord.eq
		orderings[(v,v)] = Ord.eq

		if transitive([s,t,u,v], orderings):
			res = Multiset.ord(
				make_eq(sign1, s, t),
				make_eq(sign2, u, v),
				orderings,
			)

			# print('{:2} {:2} {:2} {:2} {:2} {:2} {:2} {:2} | {:3}'.format(
			# 	'+' if sign1 else '-',
			# 	'+' if sign2 else '-',
			# 	Ord.to_sym(st), 
			# 	Ord.to_sym(uv),
			# 	Ord.to_sym(su), 
			# 	Ord.to_sym(sv), 
			# 	Ord.to_sym(tu), 
			# 	Ord.to_sym(tv), 
			# 	Ord.to_sym(res) if transitive([s,t,u,v], orderings) else 'INTRANSITIVE',
			# ))
			print('set t [|{} ; {} ; {:3}; {:3}; {:3}; {:3}; {:3}; {:3}|] {:3};'.format(
				'1' if sign1 else '0',
				'1' if sign2 else '0',
				Ord.to_string(st), 
				Ord.to_string(uv),
				Ord.to_string(su), 
				Ord.to_string(sv), 
				Ord.to_string(tu), 
				Ord.to_string(tv), 
				Ord.to_string(res),
			))

		results[(sign1,sign2,st,uv,su,sv,tu,tv)] = res


# print(Ord.to_sym(Multiset.ord([3,3,4,0], [3,4], None)))
# print(Ord.to_sym(Multiset.ord([3,3,4,0], [3,2,2,1,1,1,4,0], None)))
# print(Ord.to_sym(Multiset.ord([3,3,4,0], [3,3,3,3,2,2], None)))
# print(Ord.to_sym(Multiset.ord([3,3,4,0], [3,3,4,0], None)))
# print(Ord.to_sym(Multiset.ord([3,4], [3,3,4,0], None)))
# print(Ord.to_sym(Multiset.ord([3,2,2,1,1,1,4,0], [3,3,4,0], None)))
# print(Ord.to_sym(Multiset.ord([3,3,3,3,2,2], [3,3,4,0], None)))

# def all_same(l):
# 	return l.count(l[0]) == len(l)

# print("CONDENSED")
# for sign1, sign2 in product([True,False], repeat=2): 
# 	for st, su, sv, tu, tv, uv in product([Ord.gt,Ord.lt,eq,Ord.inc], repeat=6):
# 		foo = [results[(XX,sign2,st,su,sv,tu,tv,uv)] for XX in [True,False]]
# 		if all_same(foo):
# 			for XX in [True,False]:
# 				results[(XX,sign2,st,su,sv,tu,tv,uv)] = "XX"

# 		foo = [results[(sign1,sign2,st,su,sv,tu,tv,XX)] for XX in [s,t,u,v]]
# 		if all_same(foo):
# 			for XX in [s,t,u,v]:
# 				results[(sign1,sign2,st,su,sv,tu,tv,XX)] = "XX"
