def binky : Int a, Int b -> Int
    c : Int 
    c = a + b
    return c
end

main
    num : Int
    num = binky(4, 10)
    print num
    return 0
end