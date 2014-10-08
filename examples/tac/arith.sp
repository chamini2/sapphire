#   TAC generation - Arithmetic expressions
x, y : Int

x = 3 + 2*x / y

#
#   TAC generated
#
#  # line 2, VariableDeclaration
#  L0:                                     # next statement of the one in line 2
#  # line 2, VariableDeclaration
#  L1:                                     # next statement of the one in line 2
#  # line 4, Assign
#      $T0 := \2 * x
#      $T1 := $T0 / y
#      $T2 := \3 + $T1
#      x := $T2
#  L2:     
