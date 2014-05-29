def binarySearch :: (Int[0], Int) -> Int

Int n
print "Enter number of elements: "
read n

Int[10] myArray
print "Enter ", n," integers: "
for i in 0..(n-1) do
    read array[i]
end

print "Enter value to find"
read n

print binarySearch(array, n)

imp binarySearch(array, elem) as
    Int first = 0, last = elem-1, middle

    repeat
        middle = (first + last) / 2
    end
    until first > last do

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

