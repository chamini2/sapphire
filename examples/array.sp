Int [10] [20] arreglo
Bool[10]     barreglo
Char[2][30]  carreglo

Record Thing as
      ident :: String
    , coord :: Int[2]
end

Thing[10] marreglo

Int[12][30] a

a[1][3] = 2.0

marreglo[3].coord[0] = 1
marreglo[3].ident = "crash-site"
marreglo[3].ident[1] = "error"
marreglo[3] = "error"
