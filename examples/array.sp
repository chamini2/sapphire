Int [10] [20] arreglo
Bool[10]     barreglo
Char[2][30]  carreglo

Record Thing as
      ident :: String
    , coord :: Int[2]
end

def func :: Int, Bool -> Int
imp func (a,b) as
  if b then
    return a * 2
  else
    return a - 2
  end
end

Thing[10] arr

Int[10 + 20][2] a
# Int[10 + 20]["hola"] a

a[a[1][3]][3 + 2 * arr[1].coord[0]] = 2.0
a["hola"] = 10
arr[1].coord[0] = a[func(3,true)][func(3,false)]

arr[3].coord[0] = 1
arr[3].ident = "crash-site"
arr[3].ident[1] = "error"
arr[3] = "error"

read arr
