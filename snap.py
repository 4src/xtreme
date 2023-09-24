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
class obj: __repr__ = lambda i: printd(i.__dict__, i.__class__.__name__)
#----------------------------------------------------------------------------------------
#   _   _   | 
#  (_  (_)  |

class COL(obj):
  def __init__(i,at=0,txt=" "): i.n,i.at,i.txt = 0,at,txt
  def adds(i, a)  /: [i.add(x) for x in a]; return i
  def add(i,x)    :
    if x != "?": i.n += 1; i.add1(x)
  def dist(i,x,y) : return 1 if x=="?" and y=="?" else i.dist1(x,y)
  def bin(i,x)    : return x if x=="?" else i.bin1(x)

class SYM(COL): 
  def __init__(i, l=[], **kw): 
    super().__init__(**kw)
    i.has={}
    i.adds(l)

  def bin1(x)    : return x
  def add1(i,x)  : i.has[x] = 1 + i.has.get(x,0) 
  def mid(x)     :  return mode(i.has)
  def div(x)     :  return ent(i.has.values())
  def dist1(i,x,y): return 0 if x==y else 1

class NUM(COL):
  def __init__(i, l=[], **kw): 
    super().__init__(**kw)
    i._has,i.ok,i.w = {},True,0 if i.txt[-1]=="-" else 1
    i.adds(l)

  def add1(i,x)  : i.ok=False; i.has += [x]
  def mid(i)     : return median(i.has)
  def div(x)     : return stdev(i.has)
  def norm(i,x)  : a=i.has; return x if x=="?" else (x - a[0])/(a[-1] - a[0] + 1/BIG)
  def dist1(i,x,y):
    x, y = i.norm(x), i.norm(y)
    if x=="?": x= 0 if y>.5 else 1
    if y=="?": y= 0 if x>.5 else 1
    return abs(x-y)

  @property
  def has(i):
    if not i.ok: i._has.sort() 
    i.ok = True
    return i._has

  def bin1(i, x):
    tmp = (x - i.mid())/(i.div() + 1/BIG)
    for b,x in enumerate(NUM._breaks[the.bins]): 
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

def mode(sym)      : return max(sym, key=sym.get)
def ent(a)         : N = sum(a); return -sum(n/N*log(n/N,2) for n in a if n>0)
def stdev(num)     : return (per(num,.9) - per(num,.1))/2.56
def median(num)    : return per(num,.5)
def per(num,n=0.5) : return num[int(n*len(num))]
#-----------------------------------------------------------------------
#  ._   _        
#  |   (_)  \/\/ 

class ROW(obj)
  def __init__(i,a)     : i.cost, i._data, i.cells, i.bins = 0, None, a, a[:]
  def __gt__(row1,row2) : return row1.d2h() < row2.d2h()
  def around(i,rows)    : return sorted(rows, key=lambda j: i.dist(j))
  def d2h(i):
    i.cost, d, m = 1, 0, 0
    for col in i._data.cols.y:
      d += abs(x - col.w) ** 2
      m += 1
    return (d/m) ** .5
  
  def dist(i,j):
    m,d = 0,0
    for col in i._data.cols.x:
      m += 1
      d += col.dist(i.cells[n], j.cells[n]) ** the.bins
    return (d/m) ** (1/the.bins)
#----------------------------------------------------------------------------------------
#   _|   _.  _|_   _. 
#  (_|  (_|   |_  (_| 

class DATA(obj):
  def __init__(i,src):
    i.cols, i.rows = None, biglist()
    for row in src: i.add(row)
    return i.discretized()

  def clone(i,rows=[]):
    return DATA([ROW(i.cols.names)] + rows)

  def stats(i, cols=None, decimals=None, want="mid"):
    want = lambda c: c.mid() if want=="mid" else lambda c : c.div()
    return box(N=len(i.rows), **{col.txt : prin(want(c),decimals) for c in (cols or i.cols.y})

  def discretized(i):
    for col in enumerate(i.cols.x) for row in data.rows:
      row.bins[col.at] = col.bin(row.cells[col.at]
    return i
  
class COLS(obj):
  def __init__(i,a):
    i.all    = [(NUM if s[0].isupper() else SYM)(at=n,txt=s) for n,s in enumerate(a)] 
    i.x, i.y = {},{}
    for n,(name,col) in enumerate(zip(a, i.all)):
      if name[-1] != "X":
        if name[-1] == "!": i.klass = col
        (i.y if name[-1] in "!+-" else i.x).append(col)

  def add(di,row):
    if not i.cols: 
      i.cols = COLS(row.cells)
    else:
      row._data = row._data or i
      i.rows += [row]
      [col.add(x) for col,x in zip(i.cols.all, row.cells)]
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

def half(rows,sorting=False):
  a,b,C = half_extremes( random.sample(rows, k=min(len(rows),the.Half)))
  if sorting and better(b,a): a,b = b,a 
  rows = sorted(rows, key=lambda r: (dist(r,a)**2 + C**2 - dist(r,b)**2)/(2*C))
  mid  = int(len(rows)/2)
  return a, b, rows[:mid], rows[mid:]

def half_extremes(rows):
  n = int(len(rows)*the.Far)
  w = random.choice(rows)
  x = around(w, rows)[n]
  y = around(x, rows)[n]
  return x,y, dist(x,y)
#----------------------------------------------------------------------------------------
def prune(node0):
  prune_status(node0,True)
  rows = rows1 = node0.data.rows
  while len(rows1) > len(rows) ** the.min:
    e = prune_ents(rows1,{})
    for one in sorted([node for node,_ in visit(node0) if prune_isSubtree(node)],
                      key=lambda node1: -1 * prune_score(e,node1)]):
      if d2h(one.left.mid) != d2h(one.right.mid):
        prune_status(one.right if better(one.left.mid,  one.right.mid) else one.left, False)
        break
    tmp = [row for row in rows1 if row.alive] 
    if len(tmp) == len(rows1): print(2); break
    rows1 = tmp
    print(len(rows1))
  return rows1

def prune_score(e,node):
  a, b  = node.left.mid, node.right.mid
  diffs = [n for n in node0.data.cols.x.keys() if a.bins[n] != b.bins[n]]
  return sum(e[n]/(node.lvl+1) for n in diffs) / len(diffs)

def prune_isSubtree(node):
  n=node; return n.alive and n.left and n.left.alive and n.right and n.right.alive

def prune_status(node, status=True):
  if node:
    node.alive=status
    for row in nodes.rows: row.alive=status
    prune_status(node.left,  status)
    prune_status(node.right, status)

def prune_ents(rows,out):
  return {n: ent( adds(SYM(), [r.bins[n] for r in rows if r.alive]).values()) 
          for n in node0.data.cols.x.keys()}
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
