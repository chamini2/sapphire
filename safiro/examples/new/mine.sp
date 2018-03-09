def func : (Int from, Int to) -> Int
  arr : [100]Int

  for i in 0 .. 99 do
    arr[i] = i / from + to
  end

  accum : Int
  count : Int

  for i in from .. to do
    accum = accum + arr[i]
    count = count + 1
  end

  # promedio
  return accum / count
end

def g : ([]Char arr, Int a) -> Char
  n : Char
  n = arr[a]
  return n
end


main
  chars : [128]Char

  for i in 0..127 do
    chars[i] = chr(i)
  end

  print g(chars, func(8,80))
end

def chr : (Int n) -> Char
    case n
        when 7   do return '\a'
        when 8   do return '\b'
        when 9   do return '\t'
        when 10  do return '\n'
        when 11  do return '\v'
        when 12  do return '\f'
        when 13  do return '\r'
        when 32  do return ' '
        when 33  do return '!'
        when 34  do return '"'
        when 35  do return '#'
        when 36  do return '$'
        when 37  do return '%'
        when 38  do return '&'
        when 39  do return '\''
        when 40  do return '('
        when 41  do return ')'
        when 42  do return '*'
        when 43  do return '+'
        when 44  do return ','
        when 45  do return '-'
        when 46  do return '.'
        when 47  do return '/'
        when 48  do return '0'
        when 49  do return '1'
        when 50  do return '2'
        when 51  do return '3'
        when 52  do return '4'
        when 53  do return '5'
        when 54  do return '6'
        when 55  do return '7'
        when 56  do return '8'
        when 57  do return '9'
        when 58  do return ':'
        when 59  do return ';'
        when 60  do return '<'
        when 61  do return '='
        when 62  do return '>'
        when 63  do return '?'
        when 64  do return '@'
        when 65  do return 'A'
        when 66  do return 'B'
        when 67  do return 'C'
        when 68  do return 'D'
        when 69  do return 'E'
        when 70  do return 'F'
        when 71  do return 'G'
        when 72  do return 'H'
        when 73  do return 'I'
        when 74  do return 'J'
        when 75  do return 'K'
        when 76  do return 'L'
        when 77  do return 'M'
        when 78  do return 'N'
        when 79  do return 'O'
        when 80  do return 'P'
        when 81  do return 'Q'
        when 82  do return 'R'
        when 83  do return 'S'
        when 84  do return 'T'
        when 85  do return 'U'
        when 86  do return 'V'
        when 87  do return 'W'
        when 88  do return 'X'
        when 89  do return 'Y'
        when 90  do return 'Z'
        when 91  do return '['
        when 92  do return '\\'
        when 93  do return ']'
        when 94  do return '^'
        when 95  do return '_'
        when 96  do return '`'
        when 97  do return 'a'
        when 98  do return 'b'
        when 99  do return 'c'
        when 100 do return 'd'
        when 101 do return 'e'
        when 102 do return 'f'
        when 103 do return 'g'
        when 104 do return 'h'
        when 105 do return 'i'
        when 106 do return 'j'
        when 107 do return 'k'
        when 108 do return 'l'
        when 109 do return 'm'
        when 110 do return 'n'
        when 111 do return 'o'
        when 112 do return 'p'
        when 113 do return 'q'
        when 114 do return 'r'
        when 115 do return 's'
        when 116 do return 't'
        when 117 do return 'u'
        when 118 do return 'v'
        when 119 do return 'w'
        when 120 do return 'x'
        when 121 do return 'y'
        when 122 do return 'z'
        when 123 do return '{'
        when 124 do return '|'
        when 125 do return '}'
        when 126 do return '~'
        otherwise   return ' '
    end
end

def ord : (Char c) -> Int
  case c
    when '\a' do return 7
    when '\b' do return 8
    when '\t' do return 9
    when '\n' do return 10
    when '\v' do return 11
    when '\f' do return 12
    when '\r' do return 13
    when ' '  do return 32
    when '!'  do return 33
    when '"'  do return 34
    when '#'  do return 35
    when '$'  do return 36
    when '%'  do return 37
    when '&'  do return 38
    when '\'' do return 39
    when '('  do return 40
    when ')'  do return 41
    when '*'  do return 42
    when '+'  do return 43
    when ','  do return 44
    when '-'  do return 45
    when '.'  do return 46
    when '/'  do return 47
    when '0'  do return 48
    when '1'  do return 49
    when '2'  do return 50
    when '3'  do return 51
    when '4'  do return 52
    when '5'  do return 53
    when '6'  do return 54
    when '7'  do return 55
    when '8'  do return 56
    when '9'  do return 57
    when ':'  do return 58
    when ';'  do return 59
    when '<'  do return 60
    when '='  do return 61
    when '>'  do return 62
    when '?'  do return 63
    when '@'  do return 64
    when 'A'  do return 65
    when 'B'  do return 66
    when 'C'  do return 67
    when 'D'  do return 68
    when 'E'  do return 69
    when 'F'  do return 70
    when 'G'  do return 71
    when 'H'  do return 72
    when 'I'  do return 73
    when 'J'  do return 74
    when 'K'  do return 75
    when 'L'  do return 76
    when 'M'  do return 77
    when 'N'  do return 78
    when 'O'  do return 79
    when 'P'  do return 80
    when 'Q'  do return 81
    when 'R'  do return 82
    when 'S'  do return 83
    when 'T'  do return 84
    when 'U'  do return 85
    when 'V'  do return 86
    when 'W'  do return 87
    when 'X'  do return 88
    when 'Y'  do return 89
    when 'Z'  do return 90
    when '['  do return 91
    when '\\' do return 92
    when ']'  do return 93
    when '^'  do return 94
    when '_'  do return 95
    when '`'  do return 96
    when 'a'  do return 97
    when 'b'  do return 98
    when 'c'  do return 99
    when 'd'  do return 100
    when 'e'  do return 101
    when 'f'  do return 102
    when 'g'  do return 103
    when 'h'  do return 104
    when 'i'  do return 105
    when 'j'  do return 106
    when 'k'  do return 107
    when 'l'  do return 108
    when 'm'  do return 109
    when 'n'  do return 110
    when 'o'  do return 111
    when 'p'  do return 112
    when 'q'  do return 113
    when 'r'  do return 114
    when 's'  do return 115
    when 't'  do return 116
    when 'u'  do return 117
    when 'v'  do return 118
    when 'w'  do return 119
    when 'x'  do return 120
    when 'y'  do return 121
    when 'z'  do return 122
    when '{'  do return 123
    when '|'  do return 124
    when '}'  do return 125
    when '~'  do return 126
    otherwise    return 0
  end
end
