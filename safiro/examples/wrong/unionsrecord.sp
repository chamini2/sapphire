
Union Precision as
    integer  :: Int ,
    floating :: Float
end

Union Cosa as
      bla :: Int
    , prec :: Precision
end

X x, y; Cosa c

Precision r

x . x = 1
x . y = 2

c . prec . integer = 1

print c.bla, x . x, r.integer
