-- Binary search

Int first, last, middle, n, search;
[Int] array;

write "enter number of elements\n";
read n;

write "enter", n, "integers";

for i in [0..n-1] {
    read array[i]
}

write "value to find:";
read search;

first = 0;
last = n-1;
middle = (first + last)/2;

while (first <= last) {
    if (array[middle] < search)
       first = middle + 1;
    else if (array[middle] == search) {
       writef search, " found at location ", middle+1;
       break;
    } else
       last = middle - 1;

    middle = (first + last)/2;
}

if (first > last)
    write "couldn't find ", search
