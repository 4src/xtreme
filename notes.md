class Discretize:
  def unsuper(lst,big,iota):
    """Discretization: divide lst into bins of at least size 
    `big`, that span more than `iota`"""
    lst  = sorted(lst, key=first)
    x= lst[0][0]
    now  = o(lo=x, hi=x, n=0, y=Sym())
    all  = [now]
    for i,(x,y) in enumerate(lst):
      if i < len(lst) - big:
        if now.n >= big:
           if now.hi - now.lo > iota:
             if x != lst[i+1][0]:
               now = o(lo=x,hi=x,n=0,y=Sym())
               all += [now]
      now.n += 1
      now.hi = x
      now.y.add(y)
    return all
  
  def merge(b4):
    "Discretization: merge adjacent bins if they do not reduce the variability."
    j,tmp = 0,[]
    while j < len(b4):
      a = b4[j]
      if j < len(b4) - 1:
        b = b4[j+1]
        cy = a.y.merge(b.y)
        if cy.var()*.95 <= (a.y.var()*a.n + b.y.var()*b.n)/(a.n + b.n):
           a = o(lo=a.lo, hi=b.hi, n=a.n+b.n, y=cy)
           j += 1
      tmp += [a]
      j += 1
    return merge(tmp) if len(tmp) < len(b4) else b4
