
Union Precision as
    integer  :: Int ,
    floating :: Float
end

Union Cosa as
      pupu :: Int
    , prec :: Precision
end

X x, y; Cosa c

Precision r

x . x = 1
x . y = 2

c . prec . integer = 1

print c.pupu, x . x, r.integer
