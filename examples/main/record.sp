record A as
  a : Int
end

union B as
  a : [10]Int
, b : [10]Char
end

union Number as
  int : Int,
  float : Float
end

n : Number

main
  n.int = 1
  n.float = 1.0
end
