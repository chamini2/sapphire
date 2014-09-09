# a b c : Int
a, b, c : Int

# read "give me a " a
# read "give me b " b
# read "give me c " c
read "give me a ", a
read "give me b ", b
read               c

# to_s : _A a -> String
# '_A' could be convention to "whatever type"
# like 'a' and 'b' in map :: (a -> b) -> [a] -> [b]

# it can't be like Haskell because we name the variable next to the type
# so it would be to_s : a a -> String
# unless we make "syntactic sugar" for when there's only a var name, it's a "whatever type"

# print :: [ String ] -> IO ()
print [ "'a' vale "
      , to_s(a)
      , " y 'b + a' vale "
      , to_s(b + a)
      , " cuánto crees que vale 'b'?\n"
      ]

if (a == 10 and b % 2
    or c > 0
    or c > 1
    or c == 1
    or c == 2) then
    print "sí"
else
    print "no"
end

#############################

def d : ()
  print "hola"
end

def d_ : () -> ()
  print "chao"
end

def f : Int a -> Bool
  return a >= 0
end

def g : Bool
  return true
end

def g_ : () -> Bool
  return true
end

def i : Bool b -> ()
  if b then
    print "sí"
  else
    print "no"
  end
end

def j : ( Bool b  # paramtero 1
        , Int i   # paramtero 1
        , Char c  # paramtero 1
        ) -> Char # paramtero 1
  if (b == (i % 2 == 0) and
      i > 10) then
    print c
    return 'a'
  else
    return c
  end
end

Record Punto as
    x, y : Int
    nombre : String
end

Record Point as x, y : Int; name : String end

Union EitherIntFloat as
  int : Int
  float : Float
end

Union IntOFloat as int : Int; float : Float end

case object
    when ( syntax_error # becuyase hisa
         , lexical_error # akjsndjkandjks
         , vairable ) do
        print "hola"
    when 0 do
        print "pupú"
    otherwise
        print "\n"
end
