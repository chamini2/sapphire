#   Fibonacci
def fib :: Int -> Int 
imp fib(a) as
    case a 
        when 0, 1 do
            return 1
        otherwise 
            return fib(a - 1) + fib (a - 2)
    end
end

print fib(6)

