Int a,b,c
a = 10
case a
    when 2 do
        b = 10
    when 4 do
        b = 2
        c = 5
    otherwise
        b = 1
        c = 2
end

# c is not initialized
print b,c
