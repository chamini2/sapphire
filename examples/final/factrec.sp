# asume n >= 0
def fact : Int n -> Int
  if n == 0 then
    return 1
  else
    return fact(n-1) * n
  end
end

main
  for i in 0..20 do
    print "fact(", i, ") = ", fact(i), "\n"
  end
end
