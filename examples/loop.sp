# Tenemos muchas opciones para LOOPS
Int i;

for i in 1..10 do
  print i
end

########################

read i
while i % 2 == 0 do
  print i
  read i
end

########################

read i
until i % 2 == 1 do
  print i
  read i
end

########################

repeat
  read i
  if i % 2 == 0 then
    print i
  end
end while i % 2 == 0

########################

repeat
  read i
  unless i % 2 == 1 then
    print i
  end
end until i % 2 == 1

########################

repeat
  read i
end while i % 2 == 0 do
  print i
end

########################

repeat
  read i
end until i % 2 == 1 do
  print i
end
