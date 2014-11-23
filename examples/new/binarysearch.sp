def binarysearch : []Int array, Int size, Int elem -> Int
    first, last, middle : Int

    first = 0
    last = size - 1

    repeat
        middle = (first + last) / 2
    this until first > last do
        if array[middle] < elem then
            first = middle + 1
        elif array[middle] == elem then
            return middle
        else
            last = middle - 1
        end
    end

    return middle
end

def binarysearchrec : ([]Int array, Int first, Int last, Int elem) -> Int
    middle : Int

    middle = (first + last) / 2

    if array[middle] < elem then
        return binarysearchrec(array, middle + 1, last, elem)
    elif array[middle] == elem then
        return middle
    else
        return binarysearchrec(array, first, middle -  1, elem)
    end
end

main
    array : [1000]Int
    n, elem : Int

    read "enter number of elements (1000 tops) ", n

    for i in 0 .. n-1 do
        array[i] = i * 2
    end

    read "enter value to find ", elem

    print binarysearch(array, n, elem)
    print binarysearchrec(array, 0, n - 1, elem)
end
