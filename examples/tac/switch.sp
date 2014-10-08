#   TAC generation - Switch statement
week_day : Int

case week_day 
    when 1,2,3,4,5 do
        print "Weekday"
    when 6,7 do
        print "Weekend"
end

#
#   TAC generated
#
#  # line 2, VariableDeclaration
#  L1:                                     # next statement of the one in line 2
#  # line 4, Case
#      goto L3
#  L5:                                     # when label for `case` in line 5
#  # line 6, Print
#  L6:                                     # next statement of the one in line 6
#      goto L2
#  L7:                                     # when label for `case` in line 7
#  # line 8, Print
#  L8:                                     # next statement of the one in line 8
#      goto L2
#  L3:                                     # test label for `case`
#      if week_day == \1 goto L5
#      if week_day == \2 goto L5
#      if week_day == \3 goto L5
#      if week_day == \4 goto L5
#      if week_day == \5 goto L5
#      if week_day == \6 goto L7
#      if week_day == \7 goto L7
#  L2:                                     # next statement of the one in line 4
