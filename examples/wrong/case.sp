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
        Int c = 10
        b = 1
end

# c is not initialized
print b,c

case d
    when 2 do
        c = 10
    when 4 do
        c = 2
end

case b
    when d do
        c = 10
    when 4 do
        c = 2
end

# c is not initialized
print b,c
