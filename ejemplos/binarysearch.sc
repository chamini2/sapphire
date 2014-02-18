-- Binary search

Int n;
[Int] array;

write "enter number of elements\n";
read n;

write "enter ", n, " integers\n";

for i in [0..n-1] {
    read array[i]
}

write "value to find:";
read n;

binarySearch(array, n)

def binarySearch(array, elem) :: ([Int], Int) -> Int { -- posicion

    Int first = 0;
    Int last = elem-1;
    Int middle = (first + last)/2;

    while (first <= last) {
        if (array[middle] < elem)
            first = middle + 1;
        else if (array[middle] == elem) {
            return middle+1;
        } else
            last = middle - 1;

        middle = (first + last)/2;
    }

    -- not found
    return -1;
}
