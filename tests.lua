--  __                   __                    ___                        
-- /\ \__               /\ \__                /\_ \                       
-- \ \ ,_\    __    ____\ \ ,_\   ____        \//\ \    __  __     __     
--  \ \ \/  /'__`\ /',__\\ \ \/  /',__\         \ \ \  /\ \/\ \  /'__`\   
--   \ \ \_/\  __//\__, `\\ \ \_/\__, `\      __ \_\ \_\ \ \_\ \/\ \L\.\_ 
--    \ \__\ \____\/\____/ \ \__\/\____/     /\_\/\____\\ \____/\ \__/.\_\
--     \/__/\/____/\/___/   \/__/\/___/      \/_/\/____/ \/___/  \/__/\/_/
                                                                       
local x = require"xtrm"
local l = require"lib"
local csv,oo,push,sort,test  = l.str.csv,l.str.oo,l.list.push,l.list.sort,l.test
local map                    = l.list.map
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,ROW,NUM,SYM,COLS  = x.DATA,x.ROW,x.NUM,x.SYM,x.COLS

function test.crash_testing_survives_crashing() print(a.b.c) end

function test.fail()  return false end

function test.the_show_settings() 
  oo(x.the) end

function test.round_nums()
  for _,i in pairs{-3.1,-3.7, 10,"asda",3.1,3.7} do
    print(i, l.maths.rnd(i)) end end

function test.push_basic_test(      t)
  t={30,10,20}
  return 40 == push(sort(t), 40) end

function test.map_demo(t)
  oo(map({1,2,3}, function(x) if x > 1 then return x*10 end end)) end

function test.copy_copy_nested_strucures(t,    u)
  t={1,2,{4,5,{6,7}}}
  u=l.list.copy(t)
  t[3][3][2]=7000
  return t[3][3][2] ~= u[3][3][2] end

function test.down_sort_downwards()
  return 10== sort({{100,"dd"},{50,"cc"},{50,"bb"},{10,"aa"}},
                   l.list.lt(1))[1][1] end

function test.ksort_sort_by_keys(    n,s)
  n = 0
  function s(x) n=n+1; return #x end
  return "ox" == l.list.keysort({"dog","cats","ox","anetelope"},s)[1] and n==4 end

function test.ent_calcuate_entropy(     e)
  e= l.list.entropy{a=4,b=2,c=1}
  return 1.37 < e and e < 1.38 end

function test.rand_resetting_generates_same_randoms(     b4,t,diff)
  t,b4 = {},l.rand.seed
  for i=1,10 do t[1+#t] = l.rand.rand() end
  l.rand.seed=b4
  diff=0
  for i=1,10 do diff = diff + t[i] - l.rand.rand() end 
  return diff == 0 end

function test.any_pick_one(     t)
   t={"a","a","a","a","b","b","c"}
   t=l.rand.many(t,120)
   print(table.concat(t)) 
   print(table.concat(l.list.sort(t))) end

function test.cols()
  map( COLS({"name","Age","Weight-"}).all, oo) end

--function test.nums(     n) n=x.NUM() end
-------------------- ------------------- --------------------- -------------------- ----------
test.Run(x.the)
