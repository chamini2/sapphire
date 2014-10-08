#   Factorial
def factorial : Int a -> Int
    if a > 0 then
        return a * factorial (a - 1)
    end
    return 1
end

a : Int

print "¿A qué número quieres calculcarle el factorial?\n"

read a

print factorial(a)
