Record Precision as
    integer  :: Int ,
    floating :: Float
end

Record Thing as
      num   :: Int

    # has already been declared
    # , num   :: Float
    , prec  :: Precision
end

Union Deep as

    # has not been defined
    c :: Thing
  , p :: Precision
end

Precision r1
Deep m

# only literal
# Thing[10 + r1.integer][5] array
Thing[10][6] array

# data type of size
# Int["hola"] arreglo

# 'array[1]' si not a structure
array[1].num = 2
# cant use 'Float' in index
array[2.4][2].num = 4
# cant assign 'Int' to '[[Int]]'
array = 2

array[(m.c.prec.integer + 2) / array[3][2].num][1] = array[m.p.integer][array[1][3].num] # valid

# cannot assing 'Int' to 'Float'
m.c.prec.floating = 3
m.c.prec.integer = 2

# m is not an array
m[3] = 2

# no field 'no'
m.c.prec.no = 3

r1 = m.p

print r1, m, array
