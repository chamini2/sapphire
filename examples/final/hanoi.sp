main
  movessss(4,1,2,3)
end

def movessss : (Int n, Int from, Int to, Int via) -> ()
  if n > 0 then
    movessss(n - 1, from, via, to)
    print "mover disco de poste", from, " a poste", to, "\n";
    movessss(n-1, via, to ,from);
  end
end
