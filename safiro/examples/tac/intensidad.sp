# scope 0

def hola : Int a, Int b -> ()
    # scope 1
    print "hola, el numero es:", a+b
end

def otra : ()
    # scope 2
    for i in 0..20 do
        # scope 3
        k : Int;
        if i % 2 == 0 and i % 3 == 0 then
            # scope 4
            read k
            hola(i, i*2)
            print k
        else
            # scope 5
            for i in i..i^2 do
                # scope 6
                k : Bool;
                k = i @ 5..10;

                if k or i % 2 == 0 then
                    # scope 7
                    print "casual"
                end
            end
        end
    end

    b : Bool
    b = false
    i: Int
    i = 10

    until b do
        # scope 8
        i = i * 3 / 2
        b = i % 2 == 1;
    end
end

otra()
