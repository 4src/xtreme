#!/usr/bin/env python3 -B
# vim: set et sts=2 sw=2 ts=2 : 
import re,ast
import fileinput
from pprint import pprint as show
from math import cos,log
#----------------------------------------------------------------------------------------
class box(dict)  : __setattr__ = dict.__setitem__; __getattr__ = dict.get
class obj(object): __repr__ =lambda i:str(i.__dict__)

the=box(file="../data/auto93.csv", bins=5, Half=256, p=2)

big=1E30
#----------------------------------------------------------------------------------------
def symp(x):  return isinstance(x, dict)
def nump(x):  return isinstance(x, list)
def numsp(s): return s[0].isupper()

def COLS(a):
  x,y,all = {},{},[([] if numsp(s) else {}) for s in a] 
  w = [(0 if s[-1]=="-" else 1) for s in a]
  for n,(name,col) in enumerate(zip(a,all)):
    if name[-1] != "X":
      (y if name[-1] in "!+-" else x)[n] = col
  return box(names=a, w=w, x=x, y=y, all=all)

def add(col,x):
  def sym(): col[x] = 1 + col.get(x,0)
  def num(): col.append(x)
  if x != "?": sym() if symp(col) else num()

def norm(col,x):
  return x if (x=="?" or symp(col)) else (x - col[0])/(col[-1] - col[0] + 1/big)

def bin(col, x):
  if x=="?" or symp(col): return x 
  tmp = (x - mid(col))/(sd(col) + 1/big)
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
def ROW(a): return box(cost=0, data=None, cells=a, bins=a[:])

def dist(row1,row2):
  def sym(x,y): return 0 if x==y else 1
  def num(x,y):
    if x=="?": x= 0 if y>.5 else 1
    if y=="?": y= 0 if x>.5 else 1
    return abs(x-y)
  def dist1(col,x,y):
    if x=="?" and y=="?": return 1
    return sym(x,y) if symp(col) else num(norm(col,x), norm(col,y))
  m,d = 0,0
  for n,col in row1.data.cols.x.items():
    x  = row1.cells[n]
    y  = row2.cells[n]
    d += dist1(col,x,y)**the.bins
    m += 1
  return (d/m) ** (1/the.bins)

def d2h(row):
  row.cost = 1
  d,m = 0,0
  for n,col in row.data.cols.y.items():
    x  = norm(col, row.cells[n])
    d += abs(x - row.data.cols.w[n])**the.p
    m += 1
  return (d/m)^(1/the.p)

def better(row1,row2):
  return d2h(row1) < d2h(row2)

def around(row1,rows):
  return sorted(rows, key=lambda row2: dist(row1,row2))

def extremities(rows):
  n = int(len(rows)*the.Far)
  w = rows[0]
  x = around(w, rows)[n]
  y = around(x, rows)[n]
  return x,y, dist(x,y)

def half(rows,sorting=False):
  some  = random.sample(rows, k=min(len(rows),the.Half))
  a,b,C = extremities(some)
  mid   = int(rows/2)
  if sorting and better(b,a): a,b = b,a 
  def x(A,B): return (A**2 + C**2 - B**2)/(2*C)
  rows = sorted(rows, key=lambda r: x(dist(r,a), dist(r,b)))
  return a, b, rows[:mid], rows[mid:]
#----------------------------------------------------------------------------------------
def DATA(src):
  data1= box(cols=None, rows=[])
  for row in src:
    row.data = row.data or data1
    if data1.cols: 
      data1.rows.append(row)
      [add(col,x) for col,x in zip(data1.cols.all, row.cells)]
    else: data1.cols = COLS(row.cells)
  discretize(data1)
  return data1

def discretize(data):
  for n,col in enumerate(data.cols.all):
    if nump(col): 
      col.sort() 
      for row in data.rows:
        row.bins[n] = bin(col, row.cells[n])
#----------------------------------------------------------------------------------------
def sd(a)   : return (per(a,.9) - per(a,.1))/2.56
def mid(a)  : return per(a,.5)
def per(a,p): return a[int(p*len(a))]

def mode(d): return max(d, key=d.get)
def ent(d):
  a = d.values()
  N = sum(a)
  return -sum(n/N*log(n/N,2) for n in a if n>0)

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def csv(file="-",filter=lambda a:ROW(a)):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield filter([coerce(x) for x in line.split(",")])
#----------------------------------------------------------------------------------------
DATA(csv(the.file))
