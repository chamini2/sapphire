#   TAC generation - If statement
x, y : Int

if x < 100 or x > 200 and x /= y then
    x = 0
end

#
#   TAC generated 
#
#
#  # line 2, VariableDeclaration
#  L1:                                     # next statement of the one in line 2
#  # line 2, VariableDeclaration
#  L2:                                     # next statement of the one in line 2
#  # line 4, If
#      if x < \100 goto L4
#      goto L7
#  L7:                                     # right operand of `or`
#      if x > \200 goto L8
#      goto L5
#  L8:                                     # right operand of `and`
#      if x /= y goto L4
#      goto L5
#  L4:                                     # then label for `if`
#  # line 5, Assign
#      x := \0
#  L9:                                     # next statement of the one in line 5
#      goto L6
#  L5:                                     # else label for `if`
#  L6:                                     # end of `if`
#  L3:                                     # next statement of the one in line 4
