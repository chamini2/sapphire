def i_to_s : Int i -> String
    if i < 0
      return "-" <CONCAT> i_to_s(-1 * i)
    else
      if i < 10
        case i
          when 0 do return "0"
          when 1 do return "1"
          when 2 do return "2"
          when 3 do return "3"
          when 4 do return "4"
          when 5 do return "5"
          when 6 do return "6"
          when 7 do return "7"
          when 8 do return "8"
          when 9 do return "9"
        end
      else
        Int n = i % 10
        return i_to_s(i / 10) <CONCAT> i_to_s(n)
      end
    end
end
