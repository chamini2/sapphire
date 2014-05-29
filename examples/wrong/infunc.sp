Bool x

def func :: Int -> Bool

print func(2), x(2)

##########################

imp func(y) as
    def func :: Bool, Int -> ()
    imp func(x,y) as
        print "it is ", x
        return 2
    end

    x = true
    func(x,y)
    return true
end
