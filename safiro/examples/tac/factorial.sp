#   Factorial
def factorial : Int a -> Int
    if a > 0 then
        return a * factorial (a - 1)
    end
    return 1
end

a : Int

print "¿A qué número quieres calculcarle el factorial?\n"

read a

print factorial(a)

#
#   TAC generated
#
#  # line 2, def factorial
#      begin_function 0
#  # line 3, if a > 0
#      $T1 := a(0)
#      if $T1 > \0 goto L3
#      goto L4
#  L3:                                     # then label for `if`
#  # line 4, return a * factorial(a - 1)
#      $T2 := a(0)
#      $T4 := a(0)
#      $T5 := $T4 - \1
#      param $T5
#      $T3 := call factorial, 1
#      $T6 := $T2 * $T3
#      return $T6
#  L6:                                     # next statement of the one in line 4
#      goto L5
#  L4:                                     # else label for `if`
#  L5:                                     # end of `if`
#  L2:                                     # next statement of the one in line 3
#  # line 6, return 1
#      return \1
#  L7:                                     # next statement of the one in line 6
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 9, a : DataType Int
#  L8:                                     # next statement of the one in line 9
#  # line 11, print ¿A qué número quieres calculcarle el factorial?
#  
#  L9:                                     # next statement of the one in line 11
#  # line 13, read a
#  L10:                                    # next statement of the one in line 13
#  # line 15, print factorial(a)
#  L11:                                    # next statement of the one in line 15

