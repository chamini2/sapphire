record A as
  a : [4]Char,
  b : Int
end

record B as
  aa : [2]A,
  x  : Bool
end

union C as
  ra : A,
  is : [3][2]Int
end


c : C
b : B

print c.is[2][1]
print c.ra.b
print c.ra.a[2]
print b.aa[1].b
print b.aa[1].a[2]


# # line 1, record A
# L1:                   # next statement of line 1, record A
# # line 6, record B
# L2:                   # next statement of line 6, record B
# # line 11, union C
# L3:                   # next statement of line 11, union C
# # line 17, c : DataType C
# L4:                   # next statement of line 17, c : DataType C
# # line 18, b : DataType B
# L5:                   # next statement of line 18, b : DataType B
# # line 20, print c.is[2][1]
#   $T1 := \0
#   $T2 := \0
#   $T3 := $T1 + $T2
#   $T4 := \2
#   $T5 := \8
#   $T6 := $T5 * $T4
#   $T7 := $T3 + $T6
#   $T8 := \1
#   $T9 := \4
#   $T10 := $T9 * $T8
#   $T11 := $T7 + $T10
#   $T12 := c($T11)
#   print_int $T12
# L6:                   # next statement of line 20, print c.is[2][1]
# # line 21, print c.ra.b
#   $T13 := \0
#   $T14 := \0
#   $T15 := $T13 + $T14
#   $T16 := \4
#   $T17 := $T15 + $T16
#   $T18 := c($T17)
#   print_int $T18
# L7:                   # next statement of line 21, print c.ra.b
# # line 22, print c.ra.a[2]
#   $T19 := \0
#   $T20 := \0
#   $T21 := $T19 + $T20
#   $T22 := \0
#   $T23 := $T21 + $T22
#   $T24 := \2
#   $T25 := \1
#   $T26 := $T25 * $T24
#   $T27 := $T23 + $T26
#   $T28 := c($T27)
#   print_char $T28
# L8:                   # next statement of line 22, print c.ra.a[2]
# # line 23, print b.aa[1].b
#   $T29 := \24
#   $T30 := \0
#   $T31 := $T29 + $T30
#   $T32 := \1
#   $T33 := \8
#   $T34 := $T33 * $T32
#   $T35 := $T31 + $T34
#   $T36 := \4
#   $T37 := $T35 + $T36
#   $T38 := b($T37)
#   print_int $T38
# L9:                   # next statement of line 23, print b.aa[1].b
# # line 24, print b.aa[1].a[2]
#   $T39 := \24
#   $T40 := \0
#   $T41 := $T39 + $T40
#   $T42 := \1
#   $T43 := \8
#   $T44 := $T43 * $T42
#   $T45 := $T41 + $T44
#   $T46 := \0
#   $T47 := $T45 + $T46
#   $T48 := \2
#   $T49 := \1
#   $T50 := $T49 * $T48
#   $T51 := $T47 + $T50
#   $T52 := b($T51)
#   print_char $T52
# L10:                  # next statement of line 24, print b.aa[1].a[2]
