record A as
  a : Int,
  b : Char[100]
end
union B as
  a : A,
  b : Char[10]
end
a : A
a.a = 2
if ((a.a % 2) == 0) then # if even
  print a.a
  case a.a == 2
    when true do
      print "yei"
    otherwise
      print "false"
  end
  for i in 1..100 do
    print i
    end
  repeat
    read a.a
  end while a.a > 0 do
    print a.a
  end
  until a.a < 0 do
    break
  end
end
def c : Char d -> ()
  print d
end
c('h')
