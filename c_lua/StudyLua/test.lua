-- This doc is based on the link of https://learnxinyminutes.com/docs/lua/ 


-- this is a comment.
--[[
    multiple line comment
--]]


-----------------------------------------------
-- 1. Variables and flow control.
-----------------------------------------------


num = 42

s = 'immutable strings like python'
u = [[ Double brackets
       start and end 
       multi-line strings.]]

print('the number is ' .. num)
print(u)
t = nil

while num < 50 do 
	num = num + 1
end

if num > 40 then
	print('over 40')
elseif s ~= s then -- does not equal to
	io.write('not over 40\n')
else
	thisIsGlobal = 5
	local line = io.read()
	print('Winter is coming, ' .. line)
end

foo = anUnknownVariable -- both foo and anUnknownVariable is nil

aBoolValue = false

-- only nil and false are falsy; 0 and '' are true! not like python
if not aBoolValue then print('it was false') end

ans = aBoolValue and 'yes' or 'no' --> 'no'

karlSum = 0

for i = 1, 100 do
	karlSum = karlSum + i
end

-- Use "100, 1, -1" as the range to count down, the range is "begin, end[, step]":
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

repeat
	print('the way of the future')
	num = num - 1
until num == 0



-----------------------------------------------
-- 2. Functions.
-----------------------------------------------

function fib(n)
	if n < 2 then return 1 end
	return fib(n - 2) + fib(n - 1)
end

if 1 > 0 then print('111') else print('222') end

function adder(x)
	return function (y) return x + y end
end

a1 = adder(9)
a2 = adder(36)

print(a1(16)) --> 25
print(a1(64)) --> 100


x, y, z = 1, 2, 3, 4 -- 4 will be thrown away

function bar(a, b, c)
	print(a, b, c)
	return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod') -- print 'zaphod', nil, nil and x = 4, y = 8 the rest of numbers are discarded.

function f(x) return x * x end
f = function (x) return x * x end

-- the 'local g' decl makes g-self-references ok.
local function g(x) return math.sin(x) end
local g = function (x) return math.sin(x) end

print 'hello' -- Calls with one string param don't need parens


-----------------------------------------------
-- 3. Tables.
-----------------------------------------------

t = {key1 = 'value1', keys = false} -- Dict literals have string keys by default

print(t.key1) -- print 'value1'
t.newKey = {} -- adds a new key/value pair.
t.key2 = nil  -- Removes key2 from the table.


u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- prints 'tau'
print(u['@!#']) -- prints 'qbert'
print(u[{}])    -- prints nil instead of 1729, since {} is an object instead of string or number as a key.


-- A one-table-param function call needs no parens:
function h(x) print(x.key1) end
h{key1 = 'lua'} -- Prints 'hello lua'

for key, val in pairs(u) do
	print(key, val)
end

-- _G is a special table of all globals
print 'Begin: Print _G--------------------------------'
for key, val in pairs(_G) do
	print(key, val)
end
print(_G['_G'] == _G) -- Prints 'true'
print 'End: Print _G--------------------------------'

v = {'value1', 'value2', 1.21, 'gigawatts'}

for i = 1, #v do    -- #v is the size of list v
	print(v[i]) -- indices start at 1
end

----------------------------------------------------
-- 3.1 Metatables and metamethods.
----------------------------------------------------

-- Ignore this portion



----------------------------------------------------
-- 3.2 Class-like tables and inheritance.
----------------------------------------------------
f1 = {a = 1, b = 2}  -- Represents the fraction a/b.
f2 = {a = 2, b = 3}

-- This would fail:
-- s = f1 + f2

metafraction = {}
function metafraction.__add(f1, f2)
  sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metafraction)
setmetatable(f2, metafraction)

s = f1 + f2  -- call __add(f1, f2) on f1's metatable

-- f1, f2 have no key for their metatable, unlike
-- prototypes in js, so you must retrieve it as in
-- getmetatable(f1). The metatable is a normal table
-- with keys that Lua knows about, like __add.

-- But the next line fails since s has no metatable:
-- t = s + s
-- Class-like patterns given below would fix this.

-- An __index on a metatable overloads dot lookups:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- works! thanks, metatable
print(eatenBy)

-- Direct table lookups that fail will retry using
-- the metatable's __index value, and this recurses.

-- An __index value can also be a function(tbl, key)
-- for more customized lookups.

-- Values of __index,add, .. are called metamethods.
-- Full list. Here a is a table with the metamethod.

-- __add(a, b)                     for a + b
-- __sub(a, b)                     for a - b
-- __mul(a, b)                     for a * b
-- __div(a, b)                     for a / b
-- __mod(a, b)                     for a % b
-- __pow(a, b)                     for a ^ b
-- __unm(a)                        for -a
-- __concat(a, b)                  for a .. b
-- __len(a)                        for #a
-- __eq(a, b)                      for a == b
-- __lt(a, b)                      for a < b
-- __le(a, b)                      for a <= b
-- __index(a, b)  <fn or a table>  for a.b
-- __newindex(a, b, c)             for a.b = c
-- __call(a, ...)                  for a(...)


----------------------------------------------------
-- 3.1 Metatables and metamethods.
----------------------------------------------------

-- A table can have a metatable that gives the table
-- operator-overloadish behavior. Later we'll see
-- how metatables support js-prototype behavior.

f1 = {a = 1, b = 2}  -- Represents the fraction a/b.
f2 = {a = 2, b = 3}

-- This would fail:
-- s = f1 + f2

metafraction = {}
function metafraction.__add(f1, f2)
  sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metafraction)
setmetatable(f2, metafraction)

s = f1 + f2  -- call __add(f1, f2) on f1's metatable

-- f1, f2 have no key for their metatable, unlike
-- prototypes in js, so you must retrieve it as in
-- getmetatable(f1). The metatable is a normal table
-- with keys that Lua knows about, like __add.

-- But the next line fails since s has no metatable:
-- t = s + s
-- Class-like patterns given below would fix this.

-- An __index on a metatable overloads dot lookups:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- works! thanks, metatable

-- Direct table lookups that fail will retry using
-- the metatable's __index value, and this recurses.

-- An __index value can also be a function(tbl, key)
-- for more customized lookups.

-- Values of __index,add, .. are called metamethods.
-- Full list. Here a is a table with the metamethod.

-- __add(a, b)                     for a + b
-- __sub(a, b)                     for a - b
-- __mul(a, b)                     for a * b
-- __div(a, b)                     for a / b
-- __mod(a, b)                     for a % b
-- __pow(a, b)                     for a ^ b
-- __unm(a)                        for -a
-- __concat(a, b)                  for a .. b
-- __len(a)                        for #a
-- __eq(a, b)                      for a == b
-- __lt(a, b)                      for a < b
-- __le(a, b)                      for a <= b
-- __index(a, b)  <fn or a table>  for a.b
-- __newindex(a, b, c)             for a.b = c
-- __call(a, ...)                  for a(...)

----------------------------------------------------
-- 3.2 Class-like tables and inheritance.
----------------------------------------------------

-- Classes aren't built in; there are different ways
-- to make them using tables and metatables.

-- Explanation for this example is below it.

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  newObj = {sound = 'woof'}                -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('I say ' .. self.sound)
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'I say woof'         -- 8.

-- 1. Dog acts like a class; it's really a table.
-- 2. function tablename:fn(...) is the same as
--    function tablename.fn(self, ...)
--    The : just adds a first arg called self.
--    Read 7 & 8 below for how self gets its value.
-- 3. newObj will be an instance of class Dog.
-- 4. self = the class being instantiated. Often
--    self = Dog, but inheritance can change it.
--    newObj gets self's functions when we set both
--    newObj's metatable and self's __index to self.
-- 5. Reminder: setmetatable returns its first arg.
-- 6. The : works as in 2, but this time we expect
--    self to be an instance instead of a class.
-- 7. Same as Dog.new(Dog), so self = Dog in new().
-- 8. Same as mrDog.makeSound(mrDog); self = mrDog.

----------------------------------------------------

-- Inheritance example:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  s = self.sound .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

-- 1. LoudDog gets Dog's methods and variables.
-- 2. self has a 'sound' key from new(), see 3.
-- 3. Same as LoudDog.new(LoudDog), and converted to
--    Dog.new(LoudDog) as LoudDog has no 'new' key,
--    but does have __index = Dog on its metatable.
--    Result: seymour's metatable is LoudDog, and
--    LoudDog.__index = LoudDog. So seymour.key will
--    = seymour.key, LoudDog.key, Dog.key, whichever
--    table is the first with the given key.
-- 4. The 'makeSound' key is found in LoudDog; this
--    is the same as LoudDog.makeSound(seymour).

-- If needed, a subclass's new() is like the base's:
function LoudDog:new()
  newObj = {}
  -- set up newObj
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. Modules.
----------------------------------------------------


--[[ 
-- Suppose the file mod.lua looks like this:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('Why hello there')
  sayMyName()
end

return M
--]]

-- Another file can use mod.lua's functionality:
local mod = require('mod')  -- Run the file mod.lua.

-- require is the standard way to include modules.
-- require acts like:     (if not cached; see below)
--[[
local mod = (function ()
  <contents of mod.lua>
end)()
--]]
-- It's like mod.lua is a function body, so that
-- locals inside mod.lua are invisible outside it.

-- This works because mod here = M in mod.lua:
mod.sayHello() -- Prints: Why hello there Hrunkner

-- This is wrong; sayMyName only exists in mod.lua:
-- mod.sayMyName()  -- error

-- require's return values are cached so a file is
-- run at most once, even when require'd many times.

-- Suppose mod2.lua contains "print('Hi!')".
local a = require('mod2')  -- Prints Hi!
local b = require('mod2')  -- Doesn't print; a=b.

-- dofile is like require without caching:
dofile('mod2.lua')  --> Hi!
dofile('mod2.lua')  --> Hi! (runs it again)

-- loadfile loads a lua file but doesn't run it yet.
f = loadfile('mod2.lua')  -- Call f() to run it.

-- load is loadfile for strings.
-- (loadstring is deprecated, use load instead)
g = load('print(343)')  -- Returns a function.
g()  -- Prints out 343; nothing printed before now.




