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

BIG=1E30
class obj:       __repr__= lambda i: printd(i.__dict__, i.__class__.__name__)
class box(dict): __repr__= lambda i:printd(i); __setattr__=dict.__setitem__; __getattr__=dict.get

#----------------------------------------------------------------------------------------
#   _   _   | 
#  (_  (_)  |

class COL(obj):
  def __init__(i,at=0,txt=" "): i.n,i.at,i.txt = 0,at,txt
  def adds(i, a)  : [i.add(x) for x in a]; return i
  def add(i,x)    :
    if x != "?": i.n += 1; i.add1(x)
  def dist(i,x,y) : return 1 if x=="?" and y=="?" else i.dist1(x,y)
  def bin(i,x)    : return x if x=="?" else i.bin1(x)

class SYM(COL): 
  def __init__(i, l=[], **kw): 
    super().__init__(**kw)
    i.has={}
    i.adds(l)

  def bin1(x)     : return x
  def add1(i,x)   : i.has[x] = 1 + i.has.get(x,0) 
  def mid(x)      : return mode(i.has)
  def div(x)      : return ent(i.has)
  def dist1(i,x,y): return 0 if x==y else 1

class NUM(COL):
  def __init__(i, l=[], **kw): 
    super().__init__(**kw)
    i._has,i.ok,i.heaven = {},True,0 if i.txt[-1]=="-" else 1
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

def ent(d)       : a = d.values(); N = sum(a); return -sum(n/N*log(n/N,2) for n in a if n>0)
def mode(d)      : return max(d, key=sym.get)
def mean(a)      : return sum(a)/len(a)
def stdev(a)     : return (per(a,.9) - per(a,.1))/2.56
def median(a)    : return per(a,.5)
def per(a,n=0.5) : return a[int(n*len(a))]
#-----------------------------------------------------------------------
#  ._   _        
#  |   (_)  \/\/ 

class ROW(obj):
  def __init__(i,a)     : i.cost, i._data, i.cells, i.bins = 0, None, a, a[:]
  def __gt__(row1,row2) : return row1.d2h() < row2.d2h()
  def around(i,rows)    : return sorted(rows, key=lambda j: i.dist(j))
  def d2h(i):
    i.cost, d, m = 1, 0, 0
    for col in i._data.cols.y:
      m += 1
      d += abs(i.cells[colt.at] - col.heaven) ** 2
    return (d/m) ** .5
  
  def dist(i,j):
    m,d = 0,0
    for col in i._data.cols.x:
      m += 1
      d += col.dist(i.cells[col.at], j.cells[col.at]) ** the.bins
    return (d/m) ** (1/the.bins)
#----------------------------------------------------------------------------------------
#   _|   _.  _|_   _. 
#  (_|  (_|   |_  (_| 

class DATA(obj):
  def __init__(i,src):
    i.cols, i.rows = None, []
    for row in src: i.add(row)
    return i.discretized()

  def add(di,row):
    if not i.cols: 
      i.cols = COLS(row.cells)
    else:
      row._data = row._data or i
      i.rows += [row]
      [col.add(x) for col,x in zip(i.cols.all, row.cells)]

  def clone(i,rows=[]):
    return DATA([ROW(i.cols.names)] + rows)

  def stats(i, cols=None, decimals=None, want="mid"):
    want = lambda c: c.mid() if want=="mid" else lambda c : c.div()
    return box(N=len(i.rows), **{col.txt : prin(want(c),decimals) for c in (cols or i.cols.y})

  def discretized(i):
    for col in enumerate(i.cols.x) for row in data.rows:
      row.bins[col.at] = col.bin(row.cells[col.at]
    return i
  
def COLS(names):
  all,x,y,klass  = [],[],[],None
  for n,s in enumerate(names):
    a,z  = s[0], s[-1]       
    col  = (NUM if a.isupper() else SYM)(at=n,txt=s)
    all += [col]
    if z != "X":
      if z == "!": klass = col
      (y if z in "!+-" else x).append(col)
  return box(names=a, all=all, x=x, y=y, klass=klass)

 #----------------------------------------------------------------------------------------
#   _  |        _  _|_   _   ._ 
#  (_  |  |_|  _>   |_  (/_  |  

class NODE(obj):
  def __init__(i,data,lvl=0): i.data, i.lvl, i.left, i.right = data,lvl,None,None

  def branches(i,sorting=False):
    def _branches(data, lvl):
      node = NODE(data, lvl)
      if len(data.rows) >= 2*stop:
         _,__,left,right = i.half(data.rows, sorting)
         data.mid   = right[0]
         node.left  = _branches(i.clone(left),  lvl+1)
         node.right = _branches(i.clone(right), lvl+1)
      return node
    #------------
    stop = len(i.rows) ** the.min
    return _branches(i, 0)
  
  def branch(i):
    def _branch(rows):
      if len(rows) >= 2*stop:
        _,__,left,right = i.half(rows, True)
        rest.extend(right)
        return _branch(left)
      return rows,rest
    #-----------------
    stop = len(i.rows)**the.min
    rest = []
    return _branch(i.rows)
  
  def half(i,rows,sorting=False):
    a,b,C = i.extremes( random.sample(rows, k=min(len(rows),the.Half)))
    if sorting and b > a: a,b = b,a 
    rows = sorted(rows, key=lambda r: (r.dist(a)**2 + C**2 - r.dist(b)**2)/(2*C))
    mid  = int(len(rows)/2)
    return a, b, rows[:mid], rows[mid:]
  
  def extremes(i,rows):
    n = int(len(rows)*the.Far)
    w = random.choice(rows)
    x = around(w, rows)[n]
    y = around(x, rows)[n]
    return x,y, x.dist(y)

  def nodes(i,lvl=0):
    yield i, not(i.left or i.right)
    for kid in [i.left, i.right]:
      for a,b in kid.nodes(lvl+1):
        yield a,b

  def show(i):
    width = 4 * int(log(len(i.data.rows)**the.min,2))
    for node1,leafp in i.nodes():
      pre = '|.. ' *node1.lvl
      if node1.lvl>0 and not leafp: 
        print(f"{pre:{width}}")
      else:
        about = node1.data.stats()
        if leafp: 
          prints(f"{pre:{width}}", *about.values())
        elif node.lvl==0:
          prints(f"{' ':{width}}", *about.keys())
          prints(f"{' ':{width}}", *about.values(),"mid")
          prints(f"{' ':{width}}", *node1.data.stats(want="div").values(),"div")
#----------------------------------------------------------------------------------------
def prune(node0):
  def _setStatus(node, status=True):
    if node:
      node.alive = status
      for row in node.data.rows: row.alive=status
      _setStatus(node.left,  status)
      _setStatus(node.rleft, status)

  def _bestLiveRoots():
    return sorted([node for node,_ in node0.nodes() if _liveRoot(node)],
                  reversed=True, key=lambda node1:  _score(e,node1)])

  def _liveRoot(node):
    n=node; return n.alive and n.left and n.left.alive and n.right and n.right.alive
  
  def _score(e,node):
    a, b  = node.left.mid, node.right.mid
    diffs = [col for col in node0.data.cols.x if a.bins[col.at] != b.bins[col.at]]
    return sum(e[col.at]/(node.lvl+1) for col in diffs) / len(diffs)
   
  def _ents(rows):
    return {c.at: ent(adds(SYM([r.bins[c.at] for r in rows if r.alive]))) for c in node0.data.cols.x}
  _setStatus(node0, True)

  b4 = now = node0.data.rows
  stop = len(node0.data.rows) ** the.min
  while True:
    e = _ents(now)
    for one in _liveRoots(): 
      if d2h(one.left.mid) != d2h(one.right.mid):
        _setStatus(one.right if one.left.mid > one.right.mid else one.left, False)
        b4  = now 
        now = [row for row in b4 if row.alive] 
        if len(now) >= len(b4) or len(now) <= stop : return now
        else: break
  return now
#-------------------------------------------------------------------------------------------------
#      o   _  o  _|_ 
#  \/  |  _>  |   |_ 


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
#   _   _   ._   _|_  ._   _   | 
#  (_  (_)  | |   |_  |   (_)  | 

class CONTROL(obj):

  def __init__(i,s):
    d = {m[1]:coerce(m[2]) for m in re.finditer(r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)",__doc__)}
    i.__dict__.update(**d)

  def cli(i):
    d=i.__dict__
    for k, v in d.items():
      s = str(v)
      for j, x in enumerate(sys.argv):
        if ("-"+k[0])==x or ("--"+k)==x:
          d[k] = coerce("True" if s=="False" else ("False" if s=="True" else sys.argv[j+1]))

  def run(i,name,fun):
    d=i.__dict__
    b4=deepcopy(d)
    random.seed(the.seed)
    if bad := fun()==False: print(f"❌  FAIL : {name}") 
    for k in b4: d[k] = b4[k]
    return bad
  
  def main(i):
    i.cli()
    if i.help: print(__doc__)
    [i.run(x,funs[x]) for x in sys.argv if x in EGS.egs]
#----------------------------------------------------------------------------------------
#   _    _    _ 
#  (/_  (_|  _> 
#        _|     

class EGS:
  egs= {k:v for k,v in locals().items() if callable(v) and k[0].islower()}

  def all():
    sys.exit(sum([the.run(k,fun) for k,fun in EGS.egs.items() if k != "all"]) - 1)
  
  def fail_what_happens_when_we_fail(_):  return 1 > 2

  def the(_):   print(the)

  def stats(_): printds(DATA(csv(the.file)).stats())
  
  def dist(_): 
    d=DATA(csv(the.file))
    rows=d.around(d.rows[0], d.rows)
    for i in range(0,len(rows),30):
      print(i, rows[0].dist(rows[i]))
  
  def half(_): 
    d = DATA(csv(the.file))
    a,b,k1,k2= d.half(d.rows)
    print(len(k1),len(k2))
  
  def branches(_): 
    d = DATA(csv(the.file))
    show(d.branches(sorting=True))
  
  def sort(_):
    d = DATA(csv(the.file))
    rows = sorted(d.rows, key=lambda row: row.d2h())
    print(d.clone(rows[:50]).stats())
    print(d.clone(rows[-50:]).stats())
  
  def branch(_):
    d = DATA(csv(the.file))
    best,rest= d.branch()
    print(d.clone(best).stats())
    print(d.clone(rest).stats())
#----------------------------------------------------------------------------------------
#   _  _|_   _.  ._  _|_ 
#  _>   |_  (_|  |    |_ 

if __name__ == "__main__": the=CONTROL(__doc__); the.main()
