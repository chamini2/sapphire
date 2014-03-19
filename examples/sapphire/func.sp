def func :: Int, Bool, String -> Bool

def func (i, valid, str) as
  if i % 3 == 0 and valid then
    print str
    return true
  else
    print "didn't happen"
    return false
  end
end
