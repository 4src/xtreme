 def living(i, status=True):
    i.alive = status
    for row in i.data.rows: row.alive=status
    if i.left: i.left.living(status)
    if i.right: i.right.living(status)

  def has2LiveKids(i,now):
    e = i.ents(now)
    for node,_ in i.nodes(): 
      if node.alive and node.left and node.left.alive and node.right and node.right.alive:
        a = random.choice(node.left.data.rows)
        b = random.choice(node.right.data.rows)
        yield node.score(a,b,e),node,a,b

  def score(i,a,b,e):
     diffs = [col for col in i.data.cols.x if a.bins[col.at] != b.bins[col.at]]
     return sum(e[col.at]/(i.lvl+1) for col in diffs) / (1E-30+ len(diffs)) / (i.lvl+1)

  def ents(i,now):
    return {c.at:SYM([r.bins[c.at] for r in now if r.alive]).div() for c in i.data.cols.x}

  def prune(i):
    def _prune(now):
      if len(now) > stop: 
        if fours := [four for four in i.has2LiveKids(now)]:
          _,node,a,b = max(fours, key=lambda x: x[0])
          (node.right if a > b else node.left).living(False)
          b4  = now 
          now = [row for row in b4 if row.alive] 
          print(node.lvl, len(now))
          if len(now) < len(b4): 
            return  _prune(now)
      return now
    #-------------  
    i.living(True)
    stop = len(i.data.rows) ** the.min
    return _prune(i.data.rows)


