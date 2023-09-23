#!/usr/bin/env python3 -B
# vim: set et sts=2 sw=2 ts=2 : 
"""
snap.py: a fast way to find good options
(c) Tim Menzies <timm@ieee.org>, BSD-2 license

OPTIONS:
  -b --bins        initial number of bins = 5
  -f --file        csv data file          = "../data/auto93.csv"
  -F --Far         how far to look        = .95
  -h --help        show help              = False
  -H --Half        where to find for far  = 256
  -m --min         min size               = .5
  -p --p           distance coefficient   = 2
  -s --seed        random number seed     = 1234567891
"""
import re,sys,random,fileinput
from ast import literal_eval as scan
from pprint import pprint as pp
from copy import deepcopy
from math import cos,log
#----------------------------------------------------------------------------------------
#   _   |   _   |_    _.  |   _ 
#  (_|  |  (_)  |_)  (_|  |  _> 
#   _|                          

the=dict(**{m[1]:scan(m[2]) for m in re.finditer(r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)",__doc__)})

BIG=1E30
#----------------------------------------------------------------------------------------
#   _   _   |       ._ _   ._  
#  (_  (_)  |  |_|  | | |  | | 

def SYM(): return {}
def NUM(): return biglist()

def symp(x): return type(x) is dict
def nump(x): return type(x) is biglist

def add(col,x):
  def _sym(col): col[x] = 1 + col.get(x,0)
  def _num(col): col += [x]
  if x != "?": (_sym if symp(col) else _num)(col)
  return x

def norm(col,x):
  return x if (x=="?" or symp(col)) else (x - col[0])/(col[-1] - col[0] + 1/BIG)

def mid(x):  return median(x) if nump(x) else mode(x)
def div(x):  return stdev(x)  if nump(x) else ent(x.values())

def stdev(num)    : return (per(num,.9) - per(num,.1))/2.56
def median(num)   : return per(num,.5)
def per(num,n=0.5): return num[int(n*len(num))]

def mode(sym): return max(sym, key=sym.get)
def ent(a):
  N = sum(a)
  return -sum(n/N*log(n/N,2) for n in a if n>0)

#-----------------------------------------------------------------------
#  ._   _        
#  |   (_)  \/\/ 

def ROW(a): return box(cost=0, _data=None, cells=a, bins=a[:])

def better(row1,row2):
  return d2h(row1) < d2h(row2)

def d2h(row):
  row.cost = 1
  d,m = 0,0
  for n,col in row._data.cols.y.items():
    x  = norm(col, row.cells[n])
    d += abs(x - row._data.cols.w[n]) ** 2
    m += 1
  return (d/m) ** .5

def around(row1,rows):
  return sorted(rows, key=lambda row2: dist(row1,row2))

def dist(row1,row2):
  m,d = 0,0
  for n,col in row1._data.cols.x.items():
    m += 1
    d += _dist(col,row1.cells[n],row2.cells[n]) ** the.bins
  return (d/m) ** (1/the.bins)

# private dist function
def _dist(col,x,y):
  if x=="?" and y=="?": return 1
  elif symp(col)      : return 0 if x==y else 1
  else:
    x,y= norm(col,x), norm(col,y)
    if x=="?": x= 0 if y>.5 else 1
    if y=="?": y= 0 if x>.5 else 1
    return abs(x-y)
#----------------------------------------------------------------------------------------
#   _|   _.  _|_   _. 
#  (_|  (_|   |_  (_| 

def DATA(src):
  data = box(cols=None, rows=biglist())
  for row in src: _adds(data,row)
  return _sortedAndDiscretized(data)

def clone(data,rows=[]):
  return DATA([ROW(data.cols.names)] + rows)

def stats(data, cols="y", decimals=None, want=mid):
  return box(N=len(data.rows), **{data.cols.names[n] : prin(want(c),decimals)
                                  for n,c in data.cols[cols].items()})

# private data functions
def _cols(a):
  all = [NUM() if s[0].isupper() else SYM() for s in a] 
  w   = [0 if s[-1]=="-" else 1             for s in a]
  x,y = {},{}
  for n,(name,col) in enumerate(zip(a,all)):
    if name[-1] != "X":
      (y if name[-1] in "!+-" else x)[n] = col
  return box(names=a, w=w, x=x, y=y, all=all)

def _adds(data,row):
  if not data.cols: 
    data.cols = _cols(row.cells)
  else:
    row._data = row._data or data
    data.rows += [row]
    [add(col,x) for col,x in zip(data.cols.all, row.cells)]

def _sortedAndDiscretized(data):
  for n,col in enumerate(data.cols.all):
    if nump(col): 
      col.sort() 
      for row in data.rows:
        row.bins[n] = _bin(col, row.cells[n])
  return data

def _bin(col, x):
  if x=="?" or symp(col): return x 
  tmp = (x - mid(col))/(div(col) + 1/BIG)
  for b,x in enumerate(_breaks[the.bins]): 
    if tmp <= x: return b 
  return the.bins

_breaks= {
    3: [ -.43,	 .43],
    4: [ -.67,     0,	 .67],
    5: [ -.84,  -.25,  .25,  .84],
    6: [ -.97,	-.43,    0,	 .43,  .97],
    7: [ -1.07,	-.57,	-.18,	 .18,  .57, 1.07],
    8: [ -1.15,	-.67,	-.32, 	 0,	 .32,  .67, 1.15],
    9: [ -1.22,	-.76,	-.43,	-.14,	 .14,	 .43,  .76,	1.22],
   10: [ -1.28,	-.84,	-.52,	-.25,	   0,	 .25,  .52,	 .84,	1.28]}
#----------------------------------------------------------------------------------------
#   _  |        _  _|_   _   ._ 
#  (_  |  |_|  _>   |_  (/_  |  

def NODE(data,lvl=0): return box(data=data, lvl=lvl, left=None, right=None)

def tree(data0,sorting=False):
  stop = len(data0.rows) ** the.min
  def _grow(data, lvl):
    node = NODE(data, lvl)
    if len(data.rows) >= 2*stop:
       _,__,left,right = half(data.rows, sorting)
       data.mid   = right[0]
       node.left  = _grow(clone(data0, left),  lvl+1)
       node.right = _grow(clone(data0, right), lvl+1)
    return node
  return _grow(data0, 0)

def branch(data):
  stop = len(data.rows)**the.min
  rest = []
  def _branch(rows):
    if len(rows) >= 2*stop:
      _,__,left,right = half(rows, True)
      rest.extend(right)
      return _branch(left)
    return rows,rest
  return _branch(data.rows)

def prune(node0):
  def _score(e,node):
    mid1, mid2 = node.left.mid, node.right.mid
    diffs = [c for c in node0.data.cols.x.keys() if mid1.bins[n] != mid2.bins[n]]
    return sum(e[c]/(node.lvl+1) for c in diffs) / len(diffs)

  def _ok(node) 
    n = node
    return n.alive and n.left and n.left.alive and n.right and n.right.alive

  def _are(node,alive=True):
    if node:
      node.alive=alive
      for row in nodes.rows: row.alive=alive
      _kill(node.left,aive)
      _kill(node.right,alive)

  def _ents(a,out):
    for n in node0.data.cols.x.keys():
      tmp={}
      for row in node0.data.rows: 
        if row.alive: add(tmp, row.bin[n])
      out[n] = ent(tmp.values())
    return out
    
  _are(node0,True)
  rows = rows1 = node0.data.rows
  while True:
    rows1 = [row for row in rows1 if row.alive] 
    if len(rows1) <= len(rows) ** the.min: return rows1
    candidates = [node for node,_ in visit(node0) if _ok(node)]
    e = _ents(rows1,{})
    if not candidates: return rows1
    most = max(candidates, key=lambda node: _score(e,node))
    are(most.right if better(most.left.mid, most.right.mid) else most.left, False)

def half(rows,sorting=False):
  a,b,C = _extremes( random.sample(rows, k=min(len(rows),the.Half)))
  if sorting and better(b,a): a,b = b,a 
  rows = sorted(rows, key=lambda r: (dist(r,a)**2 + C**2 - dist(r,b)**2)/(2*C))
  mid  = int(len(rows)/2)
  return a, b, rows[:mid], rows[mid:]

def _extremes(rows):
  n = int(len(rows)*the.Far)
  w = random.choice(rows)
  x = around(w, rows)[n]
  y = around(x, rows)[n]
  return x,y, dist(x,y)
#----------------------------------------------------------------------------------------
#      o   _  o  _|_ 
#  \/  |  _>  |   |_ 

def visit(node,lvl=0):
  if node:
    yield node, not(node.left or node.right)
    for kid in [node.left, node.right]:
      for a,b in visit(kid,lvl+1):
        yield a,b
 
def show(node):
  width = 4 * int(log(len(node.data.rows)**the.min,2))
  for node1,leafp in visit(node):
    pre = '|.. ' *node1.lvl
    if node.lvl>0 and not leafp: 
      print(f"{pre:{width}}")
    else:
      about = stats(node1.data)
      if leafp: 
        prints(f"{pre:{width}}", *about.values())
      elif node.lvl==0:
        prints(f"{' ':{width}}", *about.keys())
        prints(f"{' ':{width}}", *about.values(),"mid")
        prints(f"{' ':{width}}", *stats(node1.data, want=div).values(),"div")
#----------------------------------------------------------------------------------------
#   _  _|_  ._  o  ._    _    _ 
#  _>   |_  |   |  | |  (_|  _> 
#                        _|     

def coerce(s):
  try: return scan(s)
  except Exception: return s

def csv(file="-",filter=ROW):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield filter([coerce(x) for x in line.split(",")])

def printd(d,pre=""):
   return pre+"{"+(" ".join([f":{k} {prin(v,3)}" for k,v in d.items() if k[0] != "_"]))+"}"

def printds(*d,**key):
  prints(*list(d[0].keys()),**key)
  [prints(*d1.values(),**key) for d1 in d]

def prints(*l,**key): print(*[prin(x,2) for x in l],sep="\t",**key)

def prin(x,decimals=None):
  if callable(x): return x.__name__
  if decimals is None or not isinstance(x,float): return x
  return round(x,decimals)
#----------------------------------------------------------------------------------------
#   _  |   _.   _   _   _    _ 
#  (_  |  (_|  _>  _>  (/_  _> 

class box(dict): 
  __setattr__ = dict.__setitem__
  __getattr__ = dict.get
  __repr__    = printd

class biglist(list):
  def __repr__(i): 
    return str(i) if len(i)<16 else str(i[:8])+".."+str(i[-8:])
#----------------------------------------------------------------------------------------
#  ._ _    _.  o  ._  
#  | | |  (_|  |  | | 

def cli(d):
  for k, v in d.items():
    s = str(v)
    for j, x in enumerate(sys.argv):
      if ("-"+k[0])==x or ("--"+k)==x:
        d[k] = coerce("True" if s=="False" else ("False" if s=="True" else sys.argv[j+1]))
  return d

def run(name,fun,funs):
  b4=deepcopy(the)
  random.seed(the.seed)
  if bad := fun(funs)==False: print(f"❌  FAIL : {name}") 
  for k in b4: the[k] = b4[k]
  return bad

def main(funs):
  cli(the)
  funs = {k[5:]:v for k,v in funs.items() if k[:5]=="test_"}
  [run(x,funs[x],funs) for x in sys.argv if x in funs]
#----------------------------------------------------------------------------------------
#  _|_   _    _  _|_   _ 
#   |_  (/_  _>   |_  _> 

def test_all(tests):
  sys.exit(sum([run(k,fun,tests) for k,fun in tests.items() if k != "all"]) - 1)

def test_fail_what_happens_when_we_fail(_):  return 1 > 2
def test_the(_):   print(the)
def test_stats(_): printds(stats(DATA(csv(the.file))))

def test_dist(_): 
  d=DATA(csv(the.file))
  rows=around(d.rows[0], d.rows)
  for i in range(0,len(rows),30):
    print(i, dist(rows[0],rows[i]))

def test_half(_): 
  d = DATA(csv(the.file))
  a,b,c,d= half(d.rows)
  print(len(c),len(d))

def test_tree(_): 
  d = DATA(csv(the.file))
  show(tree(d,sorting=True))

def test_sort(_):
  d = DATA(csv(the.file))
  rows = sorted(d.rows, key=d2h)
  print(stats(clone(d, rows[:50])))
  print(stats(clone(d, rows[-50:])))

def test_branch(_):
  d = DATA(csv(the.file))
  best,rest= branch(d)
  print(stats(clone(d, best)))
  print(stats(clone(d, rest)))
#----------------------------------------------------------------------------------------
#   _  _|_   _.  ._  _|_ 
#  _>   |_  (_|  |    |_ 

the=box(**the)
if __name__ == "__main__": main(locals())
