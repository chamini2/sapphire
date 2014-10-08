#   Procedure - Procedure defintion and call
def proc : Int a -> ()
    print "My arg is " , a
end

proc(10)

#
#   TAC generated
#
#  # line 2, def proc
#      begin_function 0
#  # line 3, print My arg is 
#  L2:                                     # next statement of the one in line 3
#  # line 3, print a
#  L3:                                     # next statement of the one in line 3
#      end_function
#  L1:                                     # next statement of the one in line 2
#  # line 6, proc(10)
#      param \10
#      call proc, 1
#  L4:                                     # next statement of the one in line 6
