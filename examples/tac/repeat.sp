#   Code generation - 
laps: Int
tired, too_good, giving_up : Bool

repeat
    #   This code runs on every iteration
    laps = laps + 1
end while not tired do
    if giving_up then
        break
    elif too_good then
        continue
    end
end

#
#   TAC generated
#
