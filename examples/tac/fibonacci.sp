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
#  # line 2, def fib
#      begin_function 0
#  # line 3, case a
#      $T1 := a(0)
#      goto L3
#  L5:                                     # when label for `case` in line 4
#  # line 5, return 1
#      return \1
#  L6:                                     # next statement of the one in line 5
#      goto L2
#  L4:                                     # otherwise label for `case`
#  # line 7, return fib(a - 1) + fib(a - 2)
#      $T3 := a(0)
#      $T4 := $T3 - \1
#      param $T4
#      $T2 := call fib, 1
#      $T6 := a(0)
#      $T7 := $T6 - \2
#      param $T7
#      $T5 := call fib, 1
#      $T8 := $T2 + $T5
#      return $T8
#  L7:                                     # next statement of the one in line 7
#      goto L2
#  L3:                                     # test label for `case`
#      if $T1 == \0 goto L5
#      if $T1 == \1 goto L5
#      goto L4
#  L2:                                     # next statement of the one in line 3
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 11, res : DataType Int
#  L8:                                     # next statement of the one in line 11
#  # line 12, res = fib(6)
#      param \6
#      $T9 := call fib, 1
#      res(0) := $T9
#  L9:                                     # next statement of the one in line 12
