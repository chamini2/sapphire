def func :: Int, Bool, Char -> Bool

imp func (i, valid, char) as
  if i % 3 == 0 then
    print char
    print valid
    return true
  else
    print valid
    print "didn't happen"
    return false
  end
end


print func(2,true,'c')
