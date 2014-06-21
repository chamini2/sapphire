a b c : Int

read "give me a " a
read "give me b " b
read "give me c " c
read "give me a ", a
read "give me b ", b
read "give me c ", c

print "hola, a vale " a " y b + a vale " (b + a) " cuánto crees que vale b?"
print "hola, a vale ", a " y b + a vale ", (b + a) " cuánto crees que vale b?"

if a == 10 and b % 2 or c > 0
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

def h : () -> Bool
  return false
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
    return $a
    return &a
    return 'a
    return a'
    return !a
  else
    return c
  end
end
