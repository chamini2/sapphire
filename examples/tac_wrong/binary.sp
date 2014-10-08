def binarySearch : Int[10] array, Int elem -> Int
    first, last, middle : Int

    first = 0; last = elem - 1

    repeat
        middle = (first + last) / 2
    end until first > last do

        if array[middle] < elem then
            first = middle+1
        elif array[middle] == elem then
            return middle+1
        else
            last = middle-1
        end
    end

    # not found
    return -1
end

n : Int 
print "Enter number of elements: "
read n

array : Int[10] 
print "Enter ", n," integers: "
for i in 0..(n-1) do
    read array[i]
end

print "Enter value to find"
read n

print binarySearch(array, n)
