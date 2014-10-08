#   TAC generation - function definition and call 
def foo : Int a, Int b -> Int
    return a * b
end

x, y : Int

x = foo(2*x+3, y+10)

#
#   TAC generated
#
#  # line 2, FunctionDef
#      begin_function 0
#  # line 3, Return
#      $T1 := a * b
#      return $T1
#  L2:                                     # next statement of the one in line 3
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 6, VariableDeclaration
#  L3:                                     # next statement of the one in line 6
#  # line 6, VariableDeclaration
#  L4:                                     # next statement of the one in line 6
#  # line 8, Assign
#      $T3 := y + \10
#      $T4 := \2 * x
#      $T5 := $T4 + \3
#      param $T3
#      param $T5
#      $T2 := call foo, 2
#      x := $T2
#  L5:                                     # next statement of the one in line 8
#  
