Int a,b,c

a = 10
case a
    when 2 do
        b = 10
        c = 2
    when 4 do
        b = 2
        c = 1
    otherwise
        b = 1
end

# c is not initialized
print b,c

case a
    when 2 do
        c = 10
    when 4 do
        c = 2
end

# c is not initialized
print b,c
