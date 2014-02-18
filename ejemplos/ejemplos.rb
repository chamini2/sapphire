x = 10

# el 'do' es opcional
for x in x..16 do
    puts x
end

##############################################################

puts "\nahora while\n"
# ejemplo de "el 'do' es opcional"
while x%13 != 0 and true
   puts x
   x = x+3
end

##############################################################

puts "\nahora until\n"
until x^2 > 200 do
    puts x
    x *= 3
end

##############################################################

# el 'then' es opcional
if x%2 == 0 then
    puts "par"
# ejemplo de "el 'then' es opcional"
# iguales está definida abajo
elsif iguales x%3, 0
    puts "trar"     # trar es un invento de Matteo®
else
    puts "imtrar"   # impar e intrar al mismo tiempo
end

##############################################################

unless x<0
    puts "X: " + x.to_s
end

##############################################################

def iguales(a, b)
    return (not (a != b))
end

##############################################################

# el 'puts' recibe lo que devuelva el 'case'
puts case x
when 0..500
    "de cero a quinientos"
when 200..800
    "de doscientos a ochocientos"
else
    "mucho o muy poco"
end


puts "era " + x.to_s
