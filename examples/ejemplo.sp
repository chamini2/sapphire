Int x = 10

for x in x .. 16 do
    print x
end

##############################################################

print "\nahora while\n"
# ejemplo de "el 'do' es opcional"
while x%13 /= 0 and true do
   print x
   x = x+3
end

##############################################################

print "\nahora until\n"
until x^2 > 200 do
    print x
    x *= 3
end

##############################################################

# el 'then' es opcional
if x%2 == 0 then
    print "par"
# ejemplo de "el 'then' es opcional"
# iguales está definida abajo
elif iguales x%3, 0 then
    print "trar"     # trar es un invento de Matteo®
else
    print "imtrar"   # impar e intrar al mismo tiempo
end

##############################################################

unless x<0 then
    print "X: " + to_String(x)
end

##############################################################
def iguales :: Int, Int -> Bool
imp iguales(a, b) as
    return (not (a /= b))
end

##############################################################

# el 'print' recibe lo que devuelva el 'case'
case x
when 0..500 do
    print "de cero a quinientos"
when 200..800 do
    print "de doscientos a ochocientos"
else
    print "mucho o muy poco"
end


print "era " + to_String(x)
