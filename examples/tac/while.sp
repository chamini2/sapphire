#   Code generation - While example
i,x : Int

while i < 10 or i > 20 do
    i = i + 1
end

#
#   TAC generated
#
#  # line 2, i : DataType Int
#  L1:                                     # next statement of the one in line 2
#  # line 2, x : DataType Int
#  L2:                                     # next statement of the one in line 2
#  # line 4, repeat .. while i < 10 or i > 20do .. end
#  L4:                                     # before block for `loop`
#      $T1 := i(0)
#      if $T1 < \10 goto L5
#      goto L6
#  L6:                                     # right operand of `or`
#      $T2 := i(0)
#      if $T2 > \20 goto L5
#      goto L3
#  L5:                                     # after block for `loop`
#  # line 5, i = i + 1
#      $T3 := i(0)
#      $T4 := $T3 + \1
#      i(0) := $T4
#  L7:                                     # next statement of the one in line 5
#      goto L4
#  L3:                                     # next statement of the one in line 4
