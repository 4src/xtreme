#!/usr/bin/env python3 -B
# vim: set et sts=2 sw=2 ts=2 : 
import fileinput,random,re,sys,ast
from copy import deepcopy

defaults= dict(
  Far   = .95, 
  Half  = 256, 
  file  = "../data/auto93.csv", 
  p     = 2, 
  reuse = True,
  seed  = 1234567891
)

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
  x, y, klass, all, = {}, {}, None, [COL(s) for s in words]
  for n,(col,word) in enumerate(zip(all,words)):
    if word[-1] != "X":
      if word[-1] == "!": klass=col
      (y if word[-1] in "!+-" else x)[n] = col
  return box(names=words, x=x, y=y, all=all, klass=klass)

def DATA(src):
  cols, rows = None, []
  for row in src:
    if cols: 
      [add(col,cell) for cell,col in zip(row,cols.all)]
      rows += [row]
    else: 
      cols = COLS(row)
  [col.sort() for col in cols.all if nump(col)]
  return box(rows=rows, cols=cols)

def clone(data, src=[]):
  return DATA([data.cols.names] + src)

#------------------------------------------------
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

def extremes(data,rows,x=None):
  far = int(the.Far * len(rows))
  if not x:  
    w = any(rows)
    x = around(data, w, rows)[far]
  y = around(data, x, rows)[far]
  return x, y, dist(data,x,y)

def half(data,rows,sorting=False,above=None):
  a,b,C = extremes(data, many(rows, k=min(len(rows), the.Half)), above)
  if d2h(data,a) < d2h(data,b): a,b = b,a
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
      rest.extend(right)
      return _branch(left, rest, used, above1)
    return rows,rest,used
  #-----------------
  stop = len(i.rows)**(the.min)
  return _branch(data0.rows, [], set())

def nodes(node0, lvl=0):
  yield node0, (node0.left==None and node0.right==None)
  for kid in [node0.left, node0.right]:
    if kid:
      for a,b in nodes(kid, lvl+1):
        yield a,b

def show(node0):
  width = 4 * int(log(len(node0.data.rows)**the.min,2))
  for node1,leafp in i.nodes():
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

def stats(data,decs=None,what=mid,cols=None):
   return box(N=len(data.rows), **{data.cols.names[n] : prin(what(col),decimals=decs) 
                                   for n,col in (cols or data.cols.y).items()})
#-------------------------
BIG=1E30
any=random.choice
many=random.choices

class box(dict): 
  __repr__= lambda i:printd(i); __setattr__=dict.__setitem__; __getattr__=dict.get

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def csv(file="-",filter=lambda x: x):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield filter([coerce(x) for x in line.split(",")])

def printd(d,pre=""):
  return pre+"{"+(" ".join([f":{k} {prin(v,3)}" for k,v in d.items() if k[0] != "_"]))+"}"

def prin(x,decimals=None):
  if callable(x): return x.__name__
  if decimals is None or not isinstance(x,float): return x
  return round(x,decimals)

def prints(*l,**key): print(*[prin(x,2) for x in l],sep="\t",**key)

def cli(d):
  for k, v in d.items():
    s = str(v)
    for j, x in enumerate(sys.argv):
      if ("-"+k[0])==x or ("--"+k)==x:
        d[k] = coerce("True" if s=="False" else ("False" if s=="True" else sys.argv[j+1]))
  return d

#-------------------------
class eg:
  def csv():
    for row in csv(the.file): print(row)

  def d2h():
    d = DATA(csv(the.file))
    for i in range(1,len(d.rows),100):
      print(i,d2h(d, d.rows[i]))

  def dists():
    d = DATA(csv(the.file))
    for i in range(1,len(d.rows),50):
      print(dist(d, d.rows[0], d.rows[i]))

  def sort():
    d = DATA(csv(the.file))
    rows = sorted(d.rows, key=lambda r: d2h(d,r))
    d1 = clone(d,rows[:50])
    print(stats(d1,what=mid))
    print(stats(d1,what=div))

  def half():
    d = DATA(csv(the.file))
    a,b,l,r = half(d, d.rows)
    print(len(l), len(r))
#-------------------------
def run(name,fun):
  for k in defaults: the[k] = defaults[k]
  random.seed(the.seed)
  if failed := fun()==False: print(f"❌  FAIL : {name}") 
  return failed

the = box(**defaults)
if __name__ == "__main__":
  the = cli(the)
  [run(word, vars(eg)[word]) for word in sys.argv if word in vars(eg)]
