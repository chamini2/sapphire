a : [2][3][4]Int
print a[1][2][3]

# # line 1, a : [2][3][4]DataType Int
# L1:                   # next statement of line 1, a : [2][3][4]DataType Int
# # line 2, print a[1][2][3]
#   $T1 := \0
#   $T2 := \1
#   $T3 := \48
#   $T4 := $T3 * $T2
#   $T5 := $T1 + $T4
#   $T6 := \2
#   $T7 := \16
#   $T8 := $T7 * $T6
#   $T9 := $T5 + $T8
#   $T10 := \3
#   $T11 := \4
#   $T12 := $T11 * $T10
#   $T13 := $T9 + $T12
#   $T14 := a($T13)
#   print_int $T14
# L2:                   # next statement of line 2, print a[1][2][3]
