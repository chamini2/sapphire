Int x,y,z,w
y = 20
z = 30

print y,z

if y^2 > z then
  Int v
  x = 10
  w = 5
  v = 2
else
  Int a,b
  a = 5
  if a * 2 @ 1..10 then
    Int c
    w = 1
    x = 10
    c = 3 * a
  else
    Int x
    x = 2
    b = 10
    w = 2
  end
  # debe haber un error aquí
  print x
end

# y aquí otro por la x
print x, w

#habran warnings de definicion sin uso
