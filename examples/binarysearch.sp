# RUBY CODE
# puts "enter number of elements\n"
# n = gets.to_i
# puts "enter " + n.to_s + " integers\n"
# array = Array.new
# for i in 0..(n-1) do
#     array[i] = gets.to_i
# end
# puts "enter value to find"
# n = gets.to_i
# def binarySearch(array, elem)
#     first = 0
#     last = elem-1
#     middle = (first+last) / 2
#     until first > last do
#         if array[middle] < elem then
#             first = middle+1;
#         elsif array[middle] == elem then
#             return middle+1;
#         else
#             last = middle-1;
#         end
#         middle = (first+last) / 2;
#     end
#     # not found
#     return -1
# end
# puts binarySearch(array, n)


##################### MAIN
def binarySearch :: ([Int], Int) -> Int

Int n
print "enter number of elements\n"
read n

[Int] array[n]
print "enter ", n," integers\n"
for i in 0..(n-1) do
    read array[i]
end

print "enter value to find"
read n

print binarySearch(array, n)

################## FUNCTIONS

imp binarySearch(array, elem) as
    Int first = 0, last = elem-1, middle

    repeat
        middle = (first+last) / 2
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

