#   Code generation - Jumping code
aux : Int
res : Bool

res = (65 <= aux and aux <= 90) or (97 <= aux and aux <= 122)

#
#   TAC generated
#
#  # line 2, VariableDeclaration
#  L1:                                     # next statement of the one in line 2
#  # line 3, VariableDeclaration
#  L2:                                     # next statement of the one in line 3
#  # line 5, Assign
#      if \65 <= aux goto L7
#      goto L6
#  L7:                                     # right operand of `and`
#      if aux <= \90 goto L4
#      goto L6
#  L6:                                     # right operand of `or`
#      if \97 <= aux goto L8
#      goto L5
#  L8:                                     # right operand of `and`
#      if aux <= \122 goto L4
#      goto L5
#  L4:                                     # true label for assignment
#      res := \true
#      goto L3
#  L5:                                     # false label for assignment
#      res := \false
#  L3:                                     # next statement of the one in line 5
