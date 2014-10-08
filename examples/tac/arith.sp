#   TAC generation - Arithmetic expressions
x, y : Int

x = 3 + 2*x / y

#
#   TAC generated
#

#  # line 2, x : DataType Int
#  L1:                                     # next statement of the one in line 2
#  # line 2, y : DataType Int
#  L2:                                     # next statement of the one in line 2
#  # line 4, x = 3 + 2 * x / y
#      $T1 := x(0)
#      $T2 := \2 * $T1
#      $T3 := y(4)
#      $T4 := $T2 / $T3
#      $T5 := \3 + $T4
#      x(0) := $T5
#  L3:                                     # next statement of the one in line 4
