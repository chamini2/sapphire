#   TAC generation - If-Else statement
x : Int
jynx : Bool

if x == 13 or x == 7 then
    print "Bad luck"
else
    print "Keep calm"
end

#
#   TAC generated
#
#  # line 2, x : DataType Int
#  L1:                                     # next statement of the one in line 2
#  # line 3, jynx : DataType Bool
#  L2:                                     # next statement of the one in line 3
#  # line 5, if x == 13 or x == 7
#      $T1 := x(0)
#      if $T1 == \13 goto L4
#      goto L7
#  L7:                                     # right operand of `or`
#      $T2 := x(0)
#      if $T2 == \7 goto L4
#      goto L5
#  L4:                                     # then label for `if`
#  # line 6, print Bad luck
#  L8:                                     # next statement of the one in line 6
#      goto L6
#  L5:                                     # else label for `if`
#  # line 8, print Keep calm
#  L9:                                     # next statement of the one in line 8
#  L6:                                     # end of `if`
#  L3:                                     # next statement of the one in line 5
