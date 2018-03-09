#   TAC generation - For statement
for i in 1..10 do
    print "rollin'"
    i = i + 1
end

#
#   TAC generated
#
#  # line 2, For
#      i := \1
#  L2:                                     # condition label for `for`
#      if i > \10 goto L1
#  L3:                                     # block label for `for`
#  # line 3, Print
#  L4:                                     # next statement of the one in line 3
#  # line 4, Assign
#      $T1 := i + \1
#      i := $T1
#  L5:                                     # next statement of the one in line 4
#      i := i + \1
#      goto L2
#  L1:                                     # next statement of the one in line 2
