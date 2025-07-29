# from dataclasses import dataclass
from pathlib import Path
from collections import defaultdict
import sys



try:
  prefix = sys.argv[1]
except IndexError:
  print(f'Usage: {sys.argv[0]} <dir> [<theory>]', file=sys.stderr)
  exit(1)

try:
  theory = sys.argv[2]
except IndexError:
  theory = None



def read_record(f):
  record = []
  for i in f:
    if i == "% End.\n":
      break
    _, name, *subst, bv = i.strip().split()
    n = sum(x == '1' for x in bv)
    m = len(bv)
    for _ in range(n): f.readline()
    if n == m:
      # record.append((name, subst))
      record.append(name)
  return record

theories_input = defaultdict(set)
theories_prep = defaultdict(set)
theories_sat = defaultdict(set)

for fname in Path(prefix).iterdir():
  with open(fname) as f:
    assert (f.readline() == "% Detected theories in input:\n")
    input = read_record(f)

    try:
      assert (f.readline() == "% Detected theories after preprocessing:\n")
      prep = read_record(f)
    except (AssertionError):
      prep = []

    sat = []
    while True:
      try:
        assert (f.readline() == "% Detected theories after saturation:\n")
        sat.append(read_record(f))
      except (AssertionError):
        break

    name = fname.name
    for i in input: theories_input[i].add(name)
    for i in prep: theories_prep[i].add(name)
    for j in sat: 
      for i in j: theories_sat[i].add(name)



if theory is None:
  for i in sorted(theories_input):
    a = theories_input[i]
    b = theories_prep[i]
    print(f'Input: {i} ({len(a)})')
    for j in sorted(a):
      print(j, '-' if j not in b else '')
    print()

  print('===')

  for i in sorted(theories_prep):
    a = theories_input[i]
    b = theories_prep[i]
    c = theories_sat[i]
    print()
    print(f'Prep: {i} ({len(b)} = {len(a)} + {len(b-a)} - {len(a-b)})')
    for j in sorted(b):
      print(j, '+' if j not in a else '', '-' if j not in c else '')

  print('\n===')

  for i in sorted(theories_prep):
    b = theories_prep[i]
    c = theories_sat[i]
    print()
    print(f'Sat: {i} ({len(c)} = {len(b)} + {len(c-b)} - {len(b-c)})')
    for j in sorted(c):
      print(j, '+' if j not in b else '')

else:
  a = theories_input[theory]
  b = theories_prep[theory]
  c = theories_sat[theory]

  print(f'Input: ({len(a)})')
  for j in sorted(a):
    print(j, '-' if j not in b else '')

  print()

  print(f'Prep: ({len(b)} = {len(a)} + {len(b-a)} - {len(a-b)})')
  for j in sorted(b):
    print(j, '+' if j not in a else '', '-' if j not in c else '')

  print()

  print(f'Sat: ({len(c)} = {len(b)} + {len(c-b)} - {len(b-c)})')
  for j in sorted(c):
    print(j, '+' if j not in b else '')

  print()
  
  print(f'All: ({len(a|b|c)})')
  for j in sorted(a|b|c):
    print(j)
