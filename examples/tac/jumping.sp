#   Code generation - Jumping code
aux : Int
res : Bool

res = (65 <= aux and aux <= 90) or (97 <= aux and aux <= 122)

#
#   TAC generated
#
#  # line 2, aux : DataType Int
#  L1:                                     # next statement of the one in line 2
#  # line 3, res : DataType Bool
#  L2:                                     # next statement of the one in line 3
#  # line 5, res = 65 <= aux and aux <= 90 or 97 <= aux and aux <= 122
#      $T1 := aux(0)
#      if \65 <= $T1 goto L7
#      goto L6
#  L7:                                     # right operand of `and`
#      $T2 := aux(0)
#      if $T2 <= \90 goto L4
#      goto L6
#  L6:                                     # right operand of `or`
#      $T3 := aux(0)
#      if \97 <= $T3 goto L8
#      goto L5
#  L8:                                     # right operand of `and`
#      $T4 := aux(0)
#      if $T4 <= \122 goto L4
#      goto L5
#  L4:                                     # true label for assignment
#      res(4) := \true
#      goto L3
#  L5:                                     # false label for assignment
#      res(4) := \false
#  L3:                                     # next statement of the one in line 5
