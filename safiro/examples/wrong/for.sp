Int a,b,c

a = 1+2
Range r = 0..a

for a in r do
    print a
    c = 1
end

for b in 0..10 do
    print b
    a = 10
end

# b and c are no initialized
print a, b, c
