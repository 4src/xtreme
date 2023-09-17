--  ___              __                        ___                         
-- /\_ \      __    /\ \                      /\_ \                        
-- \//\ \    /\_\   \ \ \____                 \//\ \     __  __     __     
--   \ \ \   \/\ \   \ \ '__`\                  \ \ \   /\ \/\ \  /'__`\   
--    \_\ \_  \ \ \   \ \ \L\ \            __    \_\ \_ \ \ \_\ \/\ \L\.\_ 
--    /\____\  \ \_\   \ \_,__/           /\_\   /\____\ \ \____/\ \__/.\_\
--    \/____/   \/_/    \/___/            \/_/   \/____/  \/___/  \/__/\/_/

-- | o ._  _|_ 
-- | | | |  |_ 
            
local lint={}
lint.b4={}; for k,v in pairs(_ENV) do lint.b4[k]=k end

function lint.rogues() 
  for k,v in pairs(_ENV) do 
    if not lint.b4[k] then print(string.format("E> rogue: %s of %s",k,type(v))) end end end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._   _. ._ _   _   _ 
-- | | (_| | | | (/_ _> 

local exp,sqrt,log,cos,floor,pi = math.exp,math.sqrt,math.log,math.cos,math.floor,math.pi
local list,maths,rand,settings,str,test = {},{},{},{},{},{}
local obj
-------------------- ------------------- --------------------- -------------------- ----------
--  _  |_   o  _   _ _|_  _ 
-- (_) |_)  | (/_ (_  |_ _> 
--         _|               

local id=0
local function obj(s,    t) 
  t = {_name=s} 
  t.__index = t 
  return setmetatable(t, {
     __call=function(_,...)
        id = id + 1
        local i = setmetatable({_id=id},t)
        return setmetatable(t.init(i,...) or i,t) end}) end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._ _   _. _|_ |_   _ 
-- | | | (_|  |_ | | _> 

function maths.rnd(num, numDecimalPlaces,    mult)
  if type(num) ~= "number" then return num end
  if math.floor(num) == num then return num end
  mult = 10^(numDecimalPlaces or 0)
  return floor(num * mult + 0.5) / mult end
-------------------- ------------------- --------------------- -------------------- ----------
-- | o  _ _|_ 
-- | | _>  |_ 

function list.push(t,x)
  t[1+#t] = x
  return x end

function list.map(t,fun,    u) 
  u={}; for k,v in pairs(t) do u[k] = fun(v) end; return u end

function list.kap(t,fun,    u,v1,k2) 
  u={}; for k,v in pairs(t) do v1,k1=fun(k,v); u[k1==nil and (1+#u) or k1]=v1  end
  return u end

function list.copy(t,    u)
  if type(t) ~= "table" then return t end
  u={}; for k,v in pairs(t) do u[list.copy(k)] = list.copy(v) end; 
  setmetatable(u,getmetatable(t)) 
  return u end

function list.sort(t,fun) 
  table.sort(t,fun); return t end

function list.lt(k) return function(x,y) return x[k] < y[k] end end

function list.first(t) return t[1] end
function list.second(t) return t[2] end
function list.last(t)  return t[#t] end

function list.keys(t,    i,u) 
  u={}; for k,v in pairs(t) do u[1+#u] = {k,v} end
  table.sort(u, list.lt(1))
  i=0
  return function ()
    i = i + 1
    if i <= #u then return u[i][1], u[i][2] end end end

function list.keysort(t,keyfun) 
  local _,tmp = list
  tmp = _.map(t, function(x) return {keyfun(x),x} end)
  return _.map( _.sort(tmp, _.lt(1)), _.second) end

function list.entropy(t,     e,n,_p)
  function _p(p) return p*log(p,2) end
  e,n=0,0 
  for _,v in pairs(t) do n = n+v end
  for _,v in pairs(t) do if v>0 then e = e - _p(v/n) end end
  return e end
-------------------- ------------------- --------------------- -------------------- ----------
--  _  _  _|_ _|_ o ._   _   _ 
-- _> (/_  |_  |_ | | | (_| _> 
--                       _|    

function settings.create(s,     t)
  t={_help=s}
  s:gsub("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)", function(k,v) t[k]=str.coerce(v) end)
  return t end

function settings.cli(t,     s)
  for k,v in pairs(t) do
    s=tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        t[k]= str.coerce(s=="false" and "true" or 
                         s=="true"  and "false" or arg[n+1]) end end end 
  return t end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._  _. ._   _| 
-- |  (_| | | (_| 

rand.seed = 937162211

function rand.rint(nlo,nhi) return floor(0.5 + rand.rand(nlo,nhi)) end

function rand.rand(nlo,nhi)
  nlo,nhi   = nlo or 0, nhi or 1
  rand.seed = (16807 * rand.seed) % 2147483647
  return nlo + (nhi-nlo) * rand.seed / 2147483647 end

function rand.any(t) return t[rand.rint(1,#t)] end

function rand.many(t,n) 
  u={}; for i=1,(n or #t) do u[1+#u] = rand.any(t) end 
  return u end

function rand.norm(mu,sd,     r)
  r=rand.rand
  return (mu or 0) + (sd or 1)* sqrt(-2*log(r())) * cos(2*pi*r()) end  
-------------------- ------------------- --------------------- -------------------- ----------
-- _|_  _   _ _|_ 
--  |_ (/_ _>  |_ 

function test.Run1(the, sName, fun,    b4,ok,returned)
  b4={}; for k,v in pairs(the) do b4[k]=v end
  math.randomseed(the.seed or 1234567891)
  rand.seed    = the.seed or 1234567891
  ok, returned = pcall(fun)
  for k,v in pairs(b4) do the[k]=v end
  test.Status(ok,returned,sName) end

function test.Status(ok,returned,sName)
  if ok then -- pcall terminated normally
    if returned == false then
      print("❌  FAIL : "..sName.." : returned false") 
      return true end 
  else -- pcall terminated abnormally
    print("❌  FAIL : "..sName.." : "..tostring(returned)) 
    return true end end 

function test.Run(the,     tag,fails,sep,a,b)
  fails=0
  settings.cli(the)
  if the.help then io.write(the._help .. "\nACTIONS:") end
  for name,fun in list.keys(test) do
    if name:find"^[a-z]" then
      if    the.help 
      then  a,b="\n  ","\t: "; for s in name:gmatch("([^_]+)") do io.write(a..s..b); a,b=" ","" end 
      else
        tag = name:match"(%w+)[_]?.*"
        if the.go==tag or the.go=="all" then
          fails = fails + (test.Run1(the,tag,fun) and 1 or 0)  end end end end
  if the.help then print("") end
  lint.rogues()
  os.exit(fails) end
-------------------- ------------------- --------------------- -------------------- ----------
--  _ _|_ ._ 
-- _>  |_ |  
           
str.cat = table.concat
str.fmt = string.format

function str.trim(s)  return s:match'^%s*(.*%S)' or '' end

function str.coerce(s)
  s=str.trim(s) 
  return (math.tointeger(s) or tonumber(s) or 
          s=="true" or (s ~= "false" and s)) end

function str.coerces(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=str.coerce(s1) end
  return t end

function str.csv(sFilename,fun,     src,s)
  src = io.input(sFilename)
  while true do
    s = io.read()
    if s then fun(str.coerces(s)) else return io.close(src) end end end

function str.o(x,  n,    t,x1)
  if type(x) == "function" then return "()" end
  if type(x) == "number"   then return tostring(maths.rnd(x, n or 2)) end 
  if type(x) ~= "table"    then return tostring(x) end
  t = list.kap(x, 
        function(k,v,     x1) 
          if tostring(k):sub(1,1) ~= "_" then 
            x1 = str.o(v,n)
            return #x>0 and x1 or str.fmt(":%s %s", k, x1) end end)
  return (x._name or "").. "{" .. str.cat(#x==0 and list.sort(t) or t," ") .. "}" end

function str.oo(x,  n) 
  print(str.o(x,  n)); return x end
-------------------- ------------------- --------------------- -------------------- ----------
return {lint=lint, list=list,         maths=maths, obj=obj,
        rand=rand, settings=settings, str=str,      test=test} 
