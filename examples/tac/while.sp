#   Code generation - While example
i,x : Int

while i < 10 or i > 20 do
    i = i + 1
end

#
#   TAC generated
#
#  # line 2, VariableDeclaration
#  L1:                                     # next statement of the one in line 2
#  # line 2, VariableDeclaration
#  L2:                                     # next statement of the one in line 2
#  # line 4, Loop
#  L4:                                     # before block for `loop`
#      if i < \10 goto L5
#      goto L6
#  L6:                                     # right operand of `or`
#      if i > \20 goto L5
#      goto L3
#  L5:                                     # after block for `loop`
#  # line 5, Assign
#      $T1 := i + \1
#      i := $T1
#  L7:                                     # next statement of the one in line 5
#      goto L4
#  L3:                                     # next statement of the one in line 4
