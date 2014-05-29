def a :: Int,Int -> Int

imp a (a,b) as
    return a + b
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

b (2)

# Args (Int), not ()
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
# NOT ACCEPTED:     def c ::
def c :: ()


imp c () as
  print "hello"
end

c()

Int c

print c
