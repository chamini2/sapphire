def ackermann : Int m, n -> Int
  if m == 0 then
    return n + 1
  elif m > 0 and n == 0 then
    return ackermann(m-1, 1)
  else
    return ackermann(m-1, ackermann(m, n-1))
  end
end

main
  for m in 0..3 do
    for n in 0..4 do
      print ackermann(m,n)
    end
  end

  print ackermann(4,0)
  print ackermann(4,1)
end
