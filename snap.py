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
  -r --reuse       reuse on parent node   = False
  -s --seed        random number seed     = 1234567891
"""
import re,sys,random,fileinput
from ast import literal_eval as scan
from pprint import pprint as pp
from copy import deepcopy
from math import cos,log,sqrt,pi
#----------------------------------------------------------------------------------------
#   _   |   _   |_    _.  |   _ 
#  (_|  |  (_)  |_)  (_|  |  _> 
#   _|                          

R=random.random
BIG=1E30
class obj:       __repr__= lambda i:printd(i.__dict__, i.__class__.__name__)
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

  def bin1(i,x)   : return x
  def add1(i,x)   : i.has[x] = 1 + i.has.get(x,0) 
  def mid(i)      : return mode(i.has)
  def div(i)      : return ent(i.has)
  def dist1(i,x,y): return 0 if x==y else 1

class NUM(COL):
  def __init__(i, l=[], **kw): 
    super().__init__(**kw)
    i._has,i.ok,i.heaven = [],True,0 if i.txt[-1]=="-" else 1
    i.adds(l)

  def add1(i,x)   : i.ok=False; i._has += [x]
  def mid(i)      : return median(i.has)
  def div(i)      : return stdev(i.has)
  def norm(i,x)   : a=i.has; return x if x=="?" else (x - a[0])/(a[-1] - a[0] + 1/BIG)
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
    return the.bins - 1
  
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
def mode(d)      : return max(d, key=d.get)
def mean(a)      : return sum(a)/len(a)
def stdev(a)     : return (per(a,.9) - per(a,.1))/2.56
def median(a)    : return per(a,.5)
def per(a,n=0.5) : return a[int(n*len(a))]
def normal(mu,sd): return mu+sd*sqrt(-2*log(R())) * cos(2*pi*R())
   
#-----------------------------------------------------------------------
#  ._   _        
#  |   (_)  \/\/ 

class ROW(obj):
  def __init__(i,a)     : i.alive,i._data, i.cells, i.bins = True,None, tuple(a), a[:]
  def __gt__(row1,row2) : return row1.d2h() < row2.d2h()
  def around(i,rows)    : return sorted(rows, key=lambda j: i.dist(j))
  def d2h(i):
    d, m = 0, 0
    for col in i._data.cols.y:
      x  = col.norm(i.cells[col.at])
      inc = abs(x - col.heaven) 
      d  += inc** 2
      m  += 1
    return (d/m) ** .5
  
  def dist(i,j):
    m,d = 0,0
    for col in i._data.cols.x:
      inc = col.dist(i.cells[col.at], j.cells[col.at]) ** the.bins
      d  += inc ** the.bins
      m  += 1
    return (d/m) ** (1/the.bins)
#----------------------------------------------------------------------------------------
#   _|   _.  _|_   _. 
#  (_|  (_|   |_  (_| 

def COLS(names):
  all,x,y,klass  = [],[],[],None
  for n,s in enumerate(names):
    a,z  = s[0], s[-1]       
    col  = (NUM if a.isupper() else SYM)(at=n,txt=s)
    all += [col]
    if z != "X":
      if z == "!": klass = col
      (y if z in "!+-" else x).append(col)
  return box(names=names, all=all, x=x, y=y, klass=klass)

class DATA(obj):
  def __init__(i,src):
    i.cols, i.rows = None, []
    for row in src: i.add(row)
    i.discretized()

  def add(i,row):
    if not i.cols: 
      i.cols = COLS(row.cells)
    else:
      row._data = row._data or i
      i.rows += [row]
      [col.add(x) for col,x in zip(i.cols.all, row.cells)]

  def clone(i,rows=[]):
    return DATA([ROW(i.cols.names)] + rows)

  def stats(i, cols=None, decimals=None, want="mid"):
    def val(c): return  prin(c.mid() if want=="mid" else c.div(),decimals)
    return box(N=len(i.rows), **{c.txt : val(c) for c in (cols or i.cols.y)})

  def discretized(i):
    for col in i.cols.x:
      for row in i.rows:
         row.bins[col.at] = col.bin(row.cells[col.at])
 
  def half(i,rows,sorting=False,above=None,lvl=0):
    a,b,C = i.extremes( random.sample(rows, k=min(len(rows),the.Half)), above)
    if sorting and b > a: a,b = b,a 
    mid  = int(len(rows)/2)
    rows = sorted(rows, key=lambda r: (r.dist(a)**2 + C**2 - r.dist(b)**2)/(2*C))
    return a, b, rows[:mid], rows[mid:]
  
  def extremes(i,rows,x=None):
    n = int(len(rows)*the.Far)
    if not x:
      w = random.choice(rows)
      x = w.around(rows)[n]
    y = x.around(rows)[n]
    return x,y, x.dist(y)

  def branches(i,sorting=False):
    def _branches(data, lvl):
      node = NODE(data, lvl)
      if len(data.rows) >= 2*stop:
         _,__,left,right = i.half(data.rows, sorting,lvl)
         node.left   = _branches(i.clone(left),  lvl+1)
         node.right  = _branches(i.clone(right), lvl+1)
      return node
    #------------
    stop = len(i.rows) ** the.min
    return _branches(i, 0)
  
  def branch(i,alpha=1):
    def _branch(rows,above=None):
      if len(rows) >= (2*stop*alpha):
        above1,above2,left,right = i.half(rows, True, above if the.reuse else None)
        used.add(above1)
        used.add(above2)
        rest.extend(right)
        return _branch(left,above1)
      return rows,rest,used
    #-----------------
    stop = len(i.rows)**(the.min)
    rest = []
    used = set()
    return _branch(i.rows)
#----------------------------------------------------------------------------------------
#   _  |        _  _|_   _   ._ 
#  (_  |  |_|  _>   |_  (/_  |  

class NODE(obj):
  def __init__(i,data,lvl=0): 
    i.alive,i.data,i.lvl,i.left,i.right = True,data,lvl,None,None
 
  def nodes(i,status=True,lvl=0):
    if i.alive == status:
      yield i, (i.left==None and  i.right==None)
      for kid in [i.left, i.right]:
        if kid:
          for a,b in kid.nodes(status=status,lvl=lvl+1):
            yield a,b

  def show(i,status=True):
    width = 4 * int(log(len(i.data.rows)**the.min,2))
    for node1,leafp in i.nodes(status=status):
      pre = '|.. ' * node1.lvl
      if node1.lvl>0 and not leafp: 
        print(f"{pre:{width}}")
      else:
        about = node1.data.stats()
        if node1 and leafp: 
          prints(f"{pre:{width}}", *about.values())
        elif node1.lvl==0:
          prints(f"{' ':{width}}", *about.keys())
          prints(f"{' ':{width}}", *about.values(),"mid")
          prints(f"{' ':{width}}", *node1.data.stats(want="div").values(),"div")
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

  def maybeFails(i,name,fun):
    d=i.__dict__
    b4=deepcopy(d)
    random.seed(the.seed)
    if fails := fun()==False: print(f"❌  FAIL : {name}") 
    for k in b4: d[k] = b4[k]
    return fails
  
  def main(i):
    i.cli()
    if i.help: print(__doc__)
    todo = EGS.Egs()
    [i.maybeFails(x,todo[x]) for x in sys.argv if x in todo]
#----------------------------------------------------------------------------------------
#   _    _    _ 
#  (/_  (_|  _> 
#        _|     

class EGS:
  def Egs(): return {k:v for k,v in vars(EGS).items() if k[0].islower()}

  def all():
    sys.exit(sum([the.maybeFails(k,fun) for k,fun in vars(EGS).Egs() if k != "all"]) - 1)
  
  def fail_what_happens_when_we_fail(_):  return 1 > 2

  def the():   print(the)

  def num():
    n= NUM([normal(10,2) for x in range(1000)])
    return 9.9 < n.mid() < 10.1 and 1.9< n.div() < 2.1

  def sym():
    s= SYM([x for x in "aaaabbc"])
    return s.mid() =="a" and  1.37 < s.div() < 1.38

  def csv(): [print(x.cells) for x in csv(the.file)]
  def stats(): printds(DATA(csv(the.file)).stats())
  
  def dist(): 
    d=DATA(csv(the.file))
    rows = d.rows[0].around( d.rows)
    for i in range(0,len(rows),30):
      print(i, rows[0].dist(rows[i]))
  
  def half(): 
    d = DATA(csv(the.file))
    a,b,k1,k2= d.half(d.rows)
    print(len(k1),len(k2))
  
  def branches(): 
    d = DATA(csv(the.file))
    d.branches(sorting=False).show(); print("")
    d.branches(sorting=True).show()
  
  def sort():
    d = DATA(csv(the.file))
    rows = sorted(d.rows, key=lambda row: row.d2h())
    print(d.clone(rows[:50]).stats())
    print(d.clone(rows[-50:]).stats())
  
  def branch():
    d = DATA(csv(the.file))
    best,rest,_= d.branch()
    print(d.clone(best).stats())
    print(d.clone(rest).stats())
  
  def prune():
    d0   = DATA(csv(the.file))
    tree = d0.branches(sorting=True)
    rows1= tree.prune()
    d1   = d0.clone(rows1)
    rows2,_= d1.branch()
    d2   = d0.clone(rows2)
    print(d2.stats())

  def branch2():
    for i in range(20):
      d0   = DATA(csv(the.file))
      rows1,_,used1 = d0.branch(alpha=1)
      d1   = d0.clone(rows1)
      rows2,_,used2 = d1.branch(alpha=1)
      d2   = d0.clone(rows2)
      stats= d2.stats()
      stats.cost = len(used1.union(used2))
      print(stats)
    
   
   #tree.show()
    #for node in tree.scored(): print(node.lvl)

#----------------------------------------------------------------------------------------
#   _  _|_   _.  ._  _|_ 
#  _>   |_  (_|  |    |_ 

if __name__ == "__main__": the=CONTROL(__doc__); the.main()
