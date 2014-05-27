Int x

def func :: Int -> Bool

print func(2), x(2)

################################################################################

imp func(y) as
    def func :: Int -> ()
    imp func(x) as
        print "even: ", x % 2 == 0
        return 2
    end
    x = 10
    func(x)
    return true
end
