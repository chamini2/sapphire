Record Precision as
    integer  :: Int ,
    floating :: Float
end

Record Cosa as
      num  :: Int
    , prec :: Precision
    # , noexs :: NoExiste
end

Union Mas as
    c :: Cosa
  , p :: Precision
  # , p :: Float
  # , p :: Int
end

Cosa c
Precision r1, r2
Mas m

Int[10] array

array[2] = "hola"
array = 2

c.prec.integer = 2
c.prec.floating = 3
c.prec.ppp = 3

r1 = r2

m.c.prec.integer = 2.2

print c, r1, r2, m, array
