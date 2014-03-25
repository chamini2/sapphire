# scopeId = 0
def a :: Int -> Int
Int b = 2, c

if true then
  # scopeId = 1
  c = 2
else
  # scopeId = 2
  read c
  # imp a (i) as
  #   read i
  #   return i
  # end
end

repeat
  Int b

  print a(10)  ### inicializa b, del scope 0
end until true

print b

repeat
  # scopeId = 3
  print 2
end while c > b do
  # scopeId = 4
  read b
  print a(2)
end

# scopeId = 4
# currSc = 0

imp a (i) as
  read i
  b = 10
  return i
end

def f :: Bool, Int -> Bool
def d :: () -> Int

imp f(b,f) as
  print b
  # imp d() as
  #   return 3
  # end
  return not b
end

print c, f(true, 2)
