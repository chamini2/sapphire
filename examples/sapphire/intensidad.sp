                      # scope 0

def hola :: Int, Int
imp hola(a,b) as                            # scope 1
  print "hola, el numero es:", a+b
end

# def otra :: () esto es necesario???
# imp otra as   # Void -> Void              # scope 2
def otra :: Void
imp otra() as   # Void -> Void              # scope 2
  for i in 0..20 do                       # scope 3
    Int k;
    if i % 2 == 0 and i % 3 == 0 then   # scope 4
      read k
      hola(i, i*2)
    else                                # scope 5
      for i in i..i^2 do              # scope 6
        Bool k;
        k = i @ 5..10;

        if k or i % 2 == 0 then     # scope 7
          print "casual"
        end
      end
    end
  end

  Bool b = false;

  until b do                              # scope 8
    Int i
    read i
    b = i % 2 == 1;
  end
end

otra()
