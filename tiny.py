#!/usr/bin/env python3 -B
# vim: set et sts=2 sw=2 ts=2 : 
"""
tiny: a little light learning laboratory
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

the=dict(**{m[1]:scan(m[2]) for m in re.finditer( r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)",__doc__)})

BIG=1E30
#----------------------------------------------------------------------------------------
#   _   _   |       ._ _   ._  
#  (_  (_)  |  |_|  | | |  | | 

def SYM(): return {}
def NUM(): return biglist()

def symp(x): return type(x) is dict
def nump(x): return type(x) is biglist

def add(col,x):
  def sym(col): col[x] = 1 + col.get(x,0)
  def num(col): col += [x]
  if x != "?": (sym if symp(col) else num)(col)
  return x

def norm(col,x):
  return x if (x=="?" or symp(col)) else (x - col[0])/(col[-1] - col[0] + 1/BIG)

def mid(x):  return median(x) if nump(x) else mode(x)
def div(x):  return sd(x)     if nump(x) else ent(x)

def sd(num)       : return (per(num,.9) - per(num,.1))/2.56
def median(num)   : return per(num,.5)
def per(num,n=0.5): return num[int(n*len(num))]

def mode(sym): return max(sym, key=sym.get)
def ent(sym):
  a = sym.values()
  N = sum(a)
  return -sum(n/N*log(n/N,2) for n in a if n>0)

def bin(col, x):
  if x=="?" or symp(col): return x 
  tmp = (x - mid(col))/(sd(col) + 1/BIG)
  for b,x in enumerate(breaks[the.bins]): 
    if tmp <= x: return b 
  return the.bins

breaks= {
    3: [ -.43,	 .43],
    4: [ -.67,     0,	 .67],
    5: [ -.84,  -.25,  .25,  .84],
    6: [ -.97,	-.43,    0,	 .43,  .97],
    7: [ -1.07,	-.57,	-.18,	 .18,  .57, 1.07],
    8: [ -1.15,	-.67,	-.32, 	 0,	 .32,  .67, 1.15],
    9: [ -1.22,	-.76,	-.43,	-.14,	 .14,	 .43,  .76,	1.22],
   10: [ -1.28,	-.84,	-.52,	-.25,	   0,	 .25,  .52,	 .84,	1.28]}
#----------------------------------------------------------------------------------------
#  ._   _        
#  |   (_)  \/\/ 

def ROW(a): return box(cost=0, _data=None, cells=a, bins=a[:])

def around(row1,rows):
  return sorted(rows, key=lambda row2: dist(row1,row2))

def dist(row1,row2):
  def _sym(x,y): 
    return 0 if x==y else 1
  def _num(x,y):
    if x=="?": x= 0 if y>.5 else 1
    if y=="?": y= 0 if x>.5 else 1
    return abs(x-y)
  def _dist(col,x,y):
    if x=="?" and y=="?": return 1
    return _sym(x,y) if symp(col) else _num(norm(col,x), norm(col,y))
  m,d = 0,0
  for n,col in row1._data.cols.x.items():
    x  = row1.cells[n]
    y  = row2.cells[n]
    d += _dist(col,x,y) ** the.bins
    m += 1
  return (d/m) ** (1/the.bins)

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
#----------------------------------------------------------------------------------------
#   _|   _.  _|_   _. 
#  (_|  (_|   |_  (_| 

def DATA(src):
  data = box(cols=None, rows=biglist())
  for row in src: adds(data,row)
  return sortedAndDiscretized(data)

def adds(data,row):
  if not data.cols: 
    data.cols = COLS(row.cells)
  else:
    [add(col,x) for col,x in zip(data.cols.all, row.cells)]
    row._data = row._data or data
    data.rows += [row]

def COLS(a):
  all = [NUM() if s[0].isupper() else SYM() for s in a] 
  w   = [0 if s[-1]=="-" else 1             for s in a]
  x,y = {},{}
  for n,(name,col) in enumerate(zip(a,all)):
    if name[-1] != "X":
      (y if name[-1] in "!+-" else x)[n] = col
  return box(names=a, w=w, x=x, y=y, all=all)

def clone(data,rows=[]):
  return DATA([ROW(data.cols.names)] + rows)

def sortedAndDiscretized(data):
  for n,col in enumerate(data.cols.all):
    if nump(col): 
      col.sort() 
      for row in data.rows:
        row.bins[n] = bin(col, row.cells[n])
  return data

def stats(data, cols="y", decimals=None, want=mid):
      return box(N=len(data.rows), **{data.cols.names[n] : prin(want(c),decimals)
                                      for n,c in data.cols[cols].items()})
#----------------------------------------------------------------------------------------
#   _  |        _  _|_   _   ._ 
#  (_  |  |_|  _>   |_  (/_  |  

def extremes(rows):
  n = int(len(rows)*the.Far)
  w = random.choice(rows)
  x = around(w, rows)[n]
  y = around(x, rows)[n]
  return x,y, dist(x,y)

def half(rows,sorting=False):
  a,b,C = extremes( random.sample(rows, k=min(len(rows),the.Half)))
  if sorting and better(b,a): a,b = b,a 
  rows = sorted(rows, key=lambda r: (dist(r,a)**2 + C**2 - dist(r,b)**2)/(2*C))
  mid  = int(len(rows)/2)
  return a, b, rows[:mid], rows[mid:]

def tree(data,sorting=False):
  stop = len(data.rows)**the.min
  def _grow(data1):
    node = box(data=data1,left=None,right=None)
    if len(data1.rows) >= 2*stop:
       a,b,left,right = half(data1.rows, sorting)
       node.left = _grow(clone(data, left))
       node.right = _grow(clone(data, right))
    return node
  return _grow(data)

def visit(node,lvl=0):
  if node:
    yield node, lvl, not(node.left or node.right)
    for kid in [node.left, node.right]:
      for a,b,c in visit(kid,lvl+1):
        yield a,b,c
 
def show(tree):
  width = 4 * int(log(len(tree.data.rows)**the.min,2))
  for node,lvl,leafp in visit(tree):
    pre = '|.. ' *lvl
    if lvl>0 and not leafp: 
      print(f"{pre:{width}}")
    else:
      about = stats(node.data)
      if leafp: 
        prints(f"{pre:{width}}", *about.values())
      elif lvl==0:
        prints(f"{' ':{width}}", *about.keys())
        prints(f"{' ':{width}}", *about.values())
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

def test_fail(_):  return 1 > 2
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
#----------------------------------------------------------------------------------------
#   _  _|_   _.  ._  _|_ 
#  _>   |_  (_|  |    |_ 

the=box(**the)
if __name__ == "__main__": main(locals())
