#   TAC generation - If statement
x, y : Int

if x < 100 or x > 200 and x /= y then
    x = 0
end

#
#   TAC generated 
#
#  # line 2, x : DataType Int
#  L1:                                     # next statement of the one in line 2
#  # line 2, y : DataType Int
#  L2:                                     # next statement of the one in line 2
#  # line 4, if x < 100 or x > 200 and x /= y
#      $T1 := x(0)
#      if $T1 < \100 goto L4
#      goto L7
#  L7:                                     # right operand of `or`
#      $T2 := x(0)
#      if $T2 > \200 goto L8
#      goto L5
#  L8:                                     # right operand of `and`
#      $T3 := x(0)
#      $T4 := y(4)
#      if $T3 /= $T4 goto L4
#      goto L5
#  L4:                                     # then label for `if`
#  # line 5, x = 0
#      x(0) := \0
#  L9:                                     # next statement of the one in line 5
#      goto L6
#  L5:                                     # else label for `if`
#  L6:                                     # end of `if`
#  L3:                                     # next statement of the one in line 4
