def print_int : Int i -> ()
  if i < 0 then
    print "-"
    print_int((-1) * i)
  else
    case i
    when 0 do print "0"
    when 1 do print "1"
    when 2 do print "2"
    when 3 do print "3"
    when 4 do print "4"
    when 5 do print "5"
    when 6 do print "6"
    when 7 do print "7"
    when 8 do print "8"
    when 9 do print "9"
    otherwise
      head, tail : Int
      head = i / 10
      tail = i % 10
      print_int(head)
      print_int(tail)
    end
  end
end
