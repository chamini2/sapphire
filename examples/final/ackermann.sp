def ackermann : (Int m, Int n) -> Int
  if m == 0 then
    return n + 1
  elif m > 0 and n == 0 then
    return ackermann(m-1, 1)
  else
    return ackermann(m-1, ackermann(m, n-1))
  end
end

main
  m, n : Int

  while m <= 3 do
    n = 0
    while n <= 4 do
      print m, " ", n, " ", ackermann(m,n), "\n"
      n = n + 1
    end
    m = m + 1
  end

  print 4, " ", 0, " ", ackermann(4,0), "\n"
  print 4, " ", 1, " ", ackermann(4,1), "\n"
end
