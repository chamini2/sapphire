num : Int

def binky : (Int a, Int b) -> Int
    c : Int
    c = a + b
    return c
end

main
    num = binky(4, 10)
    print num
end
