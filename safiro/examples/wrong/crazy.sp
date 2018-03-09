Int a,b,c
# used, never implemented
def d :: Int -> Bool

# has not been defined
e()

# (Int), not (Int, Int)
# Bool + Int
# b has not been initialized
a = d(2,2) + b

# used, never implemented
def f :: ()

if d(3) then
  # k has not been defined
  print a - k
end

# defined, never implemented
def k :: ()

# h is defined, never used
def h :: Int -> Int
# a is defined, never used
imp h (a) as
  # does not have a return statement
end

# used, never implemented
def e :: (Int,Int) -> Bool

# has not been intialized
print c

# (Int, Int), not (Int)
if e(2) then

  # must implement in the same scope as def
  imp e(a,b) as
    return a == b
  end

  c = 10
  # using procedure as expression
elif f() then
  c = 11
elif e(3,4) then
  c = 12
else
  # cannot use function as statement
  e(2,3)
  c = 14
end

# b has not been initialized
print c, b
