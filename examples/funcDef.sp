def a :: Int,Int -> Int

def a :: Int,Int -> Bool

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
def a :: Int,Int

# def b :: Int -> Void
def b :: Int

b (2)

# Args (Int), not ()
b ()

# procedure as an expression
print b(2)

# is not a variable
print b


# def c :: Void -> Void
# NOT ACCEPTED:     def c ::
def c :: ()

c()

Int c

print c
