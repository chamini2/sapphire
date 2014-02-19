puts "enter number of elements\n"
n = gets.to_i

puts "enter " + n.to_s + " integers\n"

array = Array.new

for i in 0..(n-1) do
    array[i] = gets.to_i
end

puts "value to find"
n = gets.to_i

def binarySearch(array, elem)

    first = 0
    last = elem-1
    middle = (first+last) / 2

    until first > last do

        if array[middle] < elem then
            first = middle+1;

        elsif array[middle] == elem then
            return middle+1;

        else
            last = middle-1;
        end

        middle = (first+last) / 2;
    end

    # not found
    return -1
end

puts binarySearch(array, n)
