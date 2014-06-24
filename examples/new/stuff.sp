a b c : Int
a, b, c : Int

read "give me a " a
read "give me b " b
read "give me c " c
read "give me a ", a
read "give me b ", b
read "give me c ", c

# to_String : _A a -> String
# '_A' could be convention to "whatever type"
# like 'a' and 'b' in map :: (a -> b) -> [a] -> [b]

# it can't be like Haskell because we name the variable next to the type
# so it would be to_String : a a -> String
# unless we make 'sugar' for when there's only a var name, it's a whatever type

# print : [ _A ] -> IO ()
print "hola, a vale " a " y b + a vale ", (b + a), " cuánto crees que vale b?"
# print : [ String ] -> IO ()
print "hola, a vale ", a " y b + a vale ", to_String(b + a), " cuánto crees que vale b?"

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
    # how do we write Char literals
    # if we remove the conventional single quotes, and use a prefix "operator"
    # we avoid users trying to write characters as strings, like 'abc'
    return 'a'
    return $a
    return &a
    return 'a
    return a'
    return !a
  else
    return c
  end
end
