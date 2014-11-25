# asume n >= 0
def fact : (Int n) -> Int
  for i in 2..n do
    n = n * i
  end

  return n
end

main
  for i in 0..20 do
    print "fact(", i, ") = ", fact(i), "\n"
  end
end
