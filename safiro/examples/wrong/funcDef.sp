def a :: Int,Int -> Int

imp a (a,b) as
    return a + b
end

# already implemented
imp a (a,c) as
    return a - c
end

# Function as a statement
a(2,3)

print a(2,3)

# Args (Int,Int), not (String, Int)
print a("hola",3)

# Args (Int,Int), not (Int, Int, Char)
print a(2,3,'c')

# Args (Int,Int), not (Int,Int,Int)
print a(2,3,4)

# is not a variable
print a

# alerady defined
def a :: Int,Int -> ()

# used but never implemented
def b :: Int

# Args (), not (Int)
b (2)

b ()

# procedure as an expression
print b(2)

# is not a variable
print b

# must define before implement
imp c () as
  print "hello"
end

# def c :: Void -> Void
def c :: ()

imp c () as
  print "hello"
end

c()

def d :: Int -> Int

if true then
  # implement in same scope
  imp d (a) as
      return a
  end
end


Int c

print c
