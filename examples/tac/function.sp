#   TAC generation - function definition and call 
def foo : Int a, Int b -> Int
    return a * b
end

x, y : Int

x = foo(2*x+3, y+10)

#
#   TAC generated
#
#  # line 2, def foo
#      begin_function 0
#  # line 3, return a * b
#      $T1 := a(0)
#      $T2 := b(-4)
#      $T3 := $T1 * $T2
#      return $T3
#  L2:                                     # next statement of the one in line 3
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 6, x : DataType Int
#  L3:                                     # next statement of the one in line 6
#  # line 6, y : DataType Int
#  L4:                                     # next statement of the one in line 6
#  # line 8, x = foo(2 * x + 3y + 10)
#      $T5 := y(4)
#      $T6 := $T5 + \10
#      $T7 := x(0)
#      $T8 := \2 * $T7
#      $T9 := $T8 + \3
#      param $T6
#      param $T9
#      $T4 := call foo, 2
#      x(0) := $T4
#  L5:                                     # next statement of the one in line 8
