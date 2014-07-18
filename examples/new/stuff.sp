# a b c : Int
a, b, c : Int

# read "give me a " a
# read "give me b " b
# read "give me c " c
read "give me a ", a
read "give me b ", b
read "give me c ", c

# to_s : _A a -> String
# '_A' could be convention to "whatever type"
# like 'a' and 'b' in map :: (a -> b) -> [a] -> [b]

# it can't be like Haskell because we name the variable next to the type
# so it would be to_s : a a -> String
# unless we make 'sugar' for when there's only a var name, it's a whatever type

# print : [ _A ] -> IO ()
# print "hola, a vale " a " y b + a vale " (b + a) " cuánto crees que vale b?\n"

# print : [ String ] -> IO ()
print "hola, a vale ", to_s(a), " y b + a vale ", to_s(b + a), " cuánto crees que vale b?\n"

if a == 10 and b % 2
  or c > 0
    then
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

def j : Bool b, Int i, Char c -> Char
  if b == (i % 2 == 0) then
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

Union OIntOFloat as int : Int; float : Float end
