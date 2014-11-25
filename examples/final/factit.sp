# asume n >= 0
def fact : (Int n) -> Int
 product, i : Int
 i = 1
 product = 1
 while i <= n do
   product = product * i
   i = i + 1
 end

 return product
end

main
   i, n : Int
   i = 1
   n = 20
   while i <= n do
       print "fact(", i, ") = ", fact(i), "\n"
       i = i + 1
   end
end
