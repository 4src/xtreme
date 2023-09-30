#!/usr/bin/env python3 -B
# vim: set et sts=2 sw=2 ts=2 : 
"""
tiny.py: a fast way to find good options
(c) Tim Menzies <timm@ieee.org>, BSD-2 license

OPTIONS:
  -b --bins   initial number of bins   = 16
  -C --Cohen  too small                = .35
  -f --file   csv data file            = "../data/auto93.csv"
  -F --Far    how far to look          = .95
  -h --help   show help                = False
  -H --Half   where to find for far    = 256
  -m --min    min size                 = .5
  -p --p      distance coefficient     = 2
  -r --reuse  do npt reuse parent node = True
  -s --seed   random number seed       = 1234567891
"""
from ast import literal_eval as make
import fileinput,random,re,sys,ast
from copy import deepcopy
from math import inf,log

class box(dict): __repr__= lambda i:printd(i); __setattr__=dict.__setitem__; __getattr__=dict.get

the=box( **{m[1]:make(m[2]) for m in re.finditer(r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)",__doc__)})
#-------------------------------------------------------------------------------------------- 
def COL(s): return [] if s[0].isupper else {}

def nump(x): return type(x) is list

def add(col,x):
  if x=="?"      : return 
  elif nump(col) : col   += [x]
  else           : col[x] = col.get(x,0) + 1

def mid(col): return median(col) if nump(col) else mode(col)
def div(col): return stdev(col)  if nump(col) else entropy(col)

def mode(d)    : return max(d, key=d.get)
def median(a)  : return per(a, .5)
def stdev(a)   : return (per(a, .9) - per(a,.1))/ 2.56
def entropy(d) : a=d.values(); N = sum(a); return -sum(n/N*log(n/N,2) for n in a if n>0)

def per(a,p=.5): return a[ int(len(a)*p) ]

def norm(a,x):
  return x if x=="?" else (x - a[0]) / (a[-1] - a[0] + 1/BIG)

def gap(col,x,y): 
  if x=="?" and y=="?": return 1
  elif nump(col): 
    x,y = norm(col,x), norm(col,y)
    if x == "?": x = 1 if y < .5 else 0 
    if y == "?": y = 1 if x < .5 else 0 
    return abs(x - y)
  else:
    return 0 if x==y else 1 

#------------------------------------------------
def COLS(words):
  x, y, klass, all = {}, {}, None, [COL(s) for s in words]
  for n,(col,word) in enumerate(zip(all,words)):
    if word[-1] != "X":
      if word[-1] == "!": klass=col
      (y if word[-1] in "!+-" else x)[n] = col
  return box(names=words, x=x, y=y, all=all, klass=klass)

def DATA(src):
  cols, rows = None, []
  for n,row in enumerate(src):
    if n==0: 
      cols = COLS(row)
    else:
      [add(col,cell) for cell,col in zip(row,cols.all)]
      rows += [row]
  [col.sort() for col in cols.all if nump(col)]
  return box(rows=rows, cols=cols)

def clone(data, src=[]):
  return DATA([data.cols.names] + src)

def stats(data,decs=None,what=mid,cols=None):
   return box(N=len(data.rows), **{data.cols.names[n] : prin(what(col),decimals=decs) 
                                   for n,col in (cols or data.cols.y).items()})

def d2h(data,row1):
  m=d=0
  for n,col in data.cols.y.items():
    w  = 0 if data.cols.names[n][-1] == "-" else 1
    m += 1
    d += abs(w - norm(col,row1[n])) ** 2
  return (d/m) ** .5

def dist(data,row1,row2):
  m=d=0
  for n,col in data.cols.x.items():
    m += 1
    d += gap(col, row1[n], row2[n]) ** the.p
  return (d/m) ** (1/the.p)

def around(data, row1, rows):
  return sorted(rows, key=lambda row2: dist(data,row1,row2))

#------------------------------------------------
def extremes(data,rows,x=None):
  far = int(the.Far * len(rows))
  if not x:  
    w = any(rows)
    x = around(data, w, rows)[far]
  y = around(data, x, rows)[far]
  return x, y, dist(data,x,y)

def half(data,rows,sorting=False,above=None):
  a,b,C = extremes(data, many(rows, k=min(len(rows), the.Half)), above)
  if sorting and d2h(data,b) < d2h(data,a): a,b = b,a
  D    = lambda r1,r2: dist(data,r1,r2)
  X    = lambda r    : (D(r,a)**2 + C**2 - D(r,b)**2) / (2*C)
  rows = sorted(rows,key=X)
  mid  = int(len(rows)/2)
  return a, b, rows[:mid], rows[mid:]

def branches(data0, sorting=False):
  def _branches(data, lvl):
    node = box(data=data, lvl=lvl, left=None, right=None)
    if len(data.rows) >= 2*stop:
      _,__,left,right = half(data0, data.rows, sorting, None)
      node.left       = _branches(clone(data0, left),  lvl+1)
      node.right      = _branches(clone(data0, right), lvl+1)
    return node
  #------------
  stop = len(data0.rows) ** the.min
  return _branches(data0, 0)
  
def branch(data0):
  def _branch(rows, rest, used, above=None):
    if len(rows) >= (2*stop):
      above1,above2,left,right = half(data0, rows, True, above if the.reuse else None)
      used.add(above1)
      used.add(above2)
      rest = rest | set(right)
      return _branch(left, rest, used, above1)
    return rows,rest,used
  #-----------------
  stop = len(data0.rows)**(the.min)
  return _branch(data0.rows, set(), set())

def branchTwice(data0):
  best1,rest1,used1 = branch(data0)
  best2,rest2,used2 = branch(clone(data0,best1))
  return best2, rest1 | rest2, used1 | used2

#------------------------------------------------
def nodes(node0, lvl=0):
  yield node0, (node0.left==None and node0.right==None)
  for kid in [node0.left, node0.right]:
    if kid:
      for a,b in nodes(kid, lvl+1):
        yield a,b

def show(node0):
  width = 4 * int(log(len(node0.data.rows)**the.min,2))
  for node1,leafp in nodes(node0):
    pre = '|.. ' * node1.lvl
    if node1.lvl>0 and not leafp: 
      print(f"{pre:{width}}")
    else:
      about = stats(node1.data)
      if node1 and leafp: 
        prints(f"{pre:{width}}", *about.values())
      elif node1.lvl==0:
        prints(f"{' ':{width}}", *about.keys())
        prints(f"{' ':{width}}", *about.values(),"mid")
        prints(f"{' ':{width}}", *stats(node1.data).values(),"div")

#------------------------------------------------------
def dull(cut): return cut[1] == -inf and cut[2] == inf

def cuts2Rule(cuts):
   d0 = defaultdict(set)
   [d0[cut[0]].add(cut) for cut in cuts]
   return tuple(sorted([tuple(sorted(x)) for x in d0.values()]))

def score(rule, d):
   got = selects(rule,d)
   b = len(got["best"]) / (len(d["best"]) + 1/big)
   r = len(got["rest"]) / (len(d["rest"]) + 1/big)
   return want[the.want](b,r)

def selects(rule, d)  : return {k: select(rule,rows) for k,rows in d.items()}
def select(rule, rows): return [row for row in rows if ands(rule,row)]

def ands(rule,row):
   for cuts in rule:
      if not ors(rows[cuts[0][0]], cuts): return False
   return True

def ors(x, cuts):
   for cut in cuts:
      if true(x, cut): return cut

def true(x, cut): return x=="?" or cut[1]==cut[2]==x or x > cut[1] and x <= cut[2]

def showRule(names,rule):
   def show(a,b): return f"{a}" if a==b else f"({a} .. {b}]"
   str = lambda cuts: ' or '.join([show(cut[1],cut[2]) for cut in cuts])
   return ' and '.join([f"{names[cuts[0][0]]}: ({str(cuts)})" for cuts in rule])

#-------------------------
def generation(data0, rows, k):
  _,_,left,right = half(data, rows, sorting=False)
  lst = left + right
  n   = len(lst) - 1
  out = []
  for _ in range(k):
    i = random.choice(range(0, n)
    if   i==0 : j = 1
    elif i==n : j = n - 1
    else      : j = i - 1 
    new = [(a+b)/2 if nump(col) else (i if R()<.5 else j)
           for col,a,b in enumerate(data0.cols.all, lst[i], lst[j])]
    out += [new]
  return out

#-------------------------
def cuts(data0, rows):
  for n,col in data0.cols.x.items():
    if not nump(col):
      for x in  {row[n] for _,rows in rows.items() for row in rows if row[n] != "?"}:
        yield (n, x, x)
    else:
      xys = [(row[n],y) for y,rows in rows.items() for row in rows if row[n] != "?"]
      p   = int(len(xys)/100)
      for one in cuts1(sorted(xys, key=lambda z:z[0]),
                       len(xys)/(the.bins - 1),
                       the.Cohen * (xys[90*p][0] - xys[10*p][0]) / 2.56):
        yield (n, one.lo, one.hi)

def cuts1(xys, nmin, xmin):
  lo  = xys[0][0]
  out = [box(lo=lo, hi=lo, ys={}, n=0)]
  for i,(x,y) in enumerate(xys):
    if i < len(xys) - nmin and x != xys[i+1][0] and out[-1].n >= nmin and x-lo >= xmin:
      out += [box(lo=hi, hi=x, y={}, n=0)]
      lo = x
    out[-1].n  += 1
    out[-1].hi  = x
    add(out[-1].y, y)
    hi = x
  out[ 0].lo = -inf
  out[-1].hi =  inf
  return merges(out)

def merges(b4):
  j,tmp = 0,[]
  while j < len(b4):
    a = b4[j]
    if j < len(b4) - 1:
      b = b4[j+1]
      if c := merged(a,b):
         a  = c
         j += 1
    tmp += [a]
    j += 1
  return merges(tmp) if len(tmp) < len(b4) else b4

def merged(a,b):
  c     = deepcopy(a)
  c.hi  = b.hi
  c.n  += b.n
  for k in b.y: c.y[k] = c.y.get(k,0) + b.y[k]
  if entropy(c.y) <= (a.n*entropy(a.y) + b.n*entropy(b.y)) / c.n:
    return c
#-------------------------
BIG=1E30
any=random.choice
many=random.choices
R=raandom.random

def coerce(s):
  try: return make(s)
  except Exception: return s

def cli(d):
  for k, v in d.items():
    s = str(v)
    for j, x in enumerate(sys.argv):
      if ("-"+k[0])==x or ("--"+k)==x:
        d[k] = coerce("True" if s=="False" else ("False" if s=="True" else sys.argv[j+1]))
  return d

def csv(file="-",filter=tuple):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield filter([coerce(x) for x in line.split(",")])

def printd(d,pre=""):
  return pre+"{"+(" ".join([f":{k} {prin(v,3)}" for k,v in d.items() if k[0] != "_"]))+" }"

def prin(x,decimals=None):
  if callable(x): return x.__name__
  if decimals is None or not isinstance(x,float): return x
  return round(x,decimals)

def prints(*l,**key): print(*[prin(x,2) for x in l],sep="\t",**key)

def printds(*d,**key):
  prints(*list(d[0].keys()),**key)
  [prints(*d1.values(),**key) for d1 in d]

#-------------------------
class EG:
  def all():
    sys.exit(sum([run(x, vars(EG)[x]) for x in sys.argv if x != "all"]))

  def settings():
    print(the)

  def csv():
    for row in csv(the.file): print(row)

  def d2h():
    d = DATA(csv(the.file))
    for i in range(1,len(d.rows),100):
      print(i,d2h(d, d.rows[i]), d.rows[i])

  def dists():
    d = DATA(csv(the.file))
    for i in range(1,len(d.rows),50):
      print(dist(d, d.rows[0], d.rows[i]))

  def sort():
    d = DATA(csv(the.file))
    rows = sorted(d.rows, key=lambda r: d2h(d,r))
    d1 = clone(d,rows[:50])
    d2 = clone(d,rows[-50:])
    printds(stats(d1),stats(d2))

  def half():
    d = DATA(csv(the.file))
    a,b,l,r = half(d, d.rows)
    print(len(l), len(r))

  def half():
    d = DATA(csv(the.file))
    a,b,l,r = half(d, d.rows)
    print(len(l), len(r))

  def tree():
    d = DATA(csv(the.file))
    show(branches(d,True))

  def optimize():
    for _ in range(20):
      d = DATA(csv(the.file))
      rows,_,_ = branch(d)
      print(stats(clone(d,rows)))

  def optimize2():
    for _ in range(20):
      d = DATA(csv(the.file))
      rows,_,_ = branchTwice(d)
      print(stats(clone(d,rows)))

#-------------------------
def run(name,fun):
  b4 = {k:v for k,v in the.items()}
  random.seed(the.seed)
  if failed := fun()==False: 
    print(f"❌  FAIL : {name}") 
  for k in b4: the[k] = b4[k]
  return failed

if __name__ == "__main__":
  the = cli(the)
  [run(arg, vars(EG)[arg]) for arg in sys.argv if arg in vars(EG)]
