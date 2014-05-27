
Union Precision as
    integer  :: Int ,
    floating :: Float
end

def odd :: Int -> Bool
imp odd(a) as
  Union Precision as
      integer  :: Int ,
      floating :: Float
  end

  return (a % 2 == 1)
end

def even :: Int -> Bool
imp even(a) as

  Record Precision as
    x :: Char,
    y :: Float,
    z :: Bool
  end

  return (a % 2 == 0)
end

Union Cosa as
      pupu :: Int
    , prec :: Precision
end

X x, y; Cosa c

Precision r

# Tipo a

# x . x = 1
# x . y = 2

# c . prec . integer = 1

# print c.pupu, x . x, r.integer
