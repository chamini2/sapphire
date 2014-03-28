def factorial :: Int -> Int
imp factorial (a) as
    if a > 0 then
        return a * factorial (a - 1)
    end
    return 1
end

print factorial(6)
