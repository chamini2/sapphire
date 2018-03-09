#   Fibonacci
def fib : Int a -> Int 
    case a 
        when 0, 1 do
            return 1
        otherwise 
            return fib(a - 1) + fib (a - 2)
    end
end

main
    return fib(6)
end
