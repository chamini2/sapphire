#   Fibonacci
def fib : Int a -> Int 
    case a 
        when 0, 1 do
            return 1
        otherwise 
            return fib(a - 1) + fib (a - 2)
    end
end

res: Int
res = fib(6)

#
#   TAC generated
#
#  # line 2, FunctionDef
#      begin_function 0
#  # line 3, Case
#      goto L3
#  L5:                                     # when label for `case` in line 4
#  # line 5, Return
#      return \1
#  L6:                                     # next statement of the one in line 5
#      goto L2
#  L4:                                     # otherwise label for `case`
#  # line 7, Return
#      $T2 := a - \1
#      param $T2
#      $T1 := call fib, 1
#      $T4 := a - \2
#      param $T4
#      $T3 := call fib, 1
#      $T5 := $T1 + $T3
#      return $T5
#  L7:                                     # next statement of the one in line 7
#      goto L2
#  L3:                                     # test label for `case`
#      if a == \0 goto L5
#      if a == \1 goto L5
#      goto L4
#  L2:                                     # next statement of the one in line 3
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 11, VariableDeclaration
#  L8:                                     # next statement of the one in line 11
#  # line 12, Assign
#      param \6
#      $T6 := call fib, 1
#      res := $T6
#  L9:                                     # next statement of the one in line 12
