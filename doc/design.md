# Sapphire programming language

## TO-DO

- [ ] Functions

## Program

A program is composed of the statements inside a block enclosed by `main` and `end`

```
main
    <statements..>
end
```

## Comments

Comments start with `#` and run until the end of the line

```
# this is a comment
```

## Identifiers

The identifiers start with a letter or underscode and continue with letters, underscores or numbers.

- `foo`
- `_BAR`
- `baz0`
- `_1Qux`

Some words are reserved for constructions of the language, these cannot be used as identifiers.

## Statements

### Statements block

Several statements can be put executed in order by using a block

```
begin
    <statements..>
end
```

Notice that a program is really just a statements block with the special name `main`

### Variable declaration

Declare a variable by naming it, setting it's data type

```
integer current_year
```

That is

```
<type> <identifier>
```

### Assignment

You assign a value to a variable by using `:=`.

```
current_year := 2018
```

The syntax would be

```
<identifier> := <expression>
```

### Input

You read from the standard input by using the statement `read`.

```
read born_in
```

Generalized as

```
read <identifier>
```

### Output

You write to standard output by using the statement `write`.

```
write "Hello, world!"
```

It may one or many arguments separated by commas. It accepts any data type from the language.

```
write "You are ", current_year - born_in, " or ", current_year - born_in - 1, "years old"
```

That is

```
write <expressions..>
```

### `if-then` and `if-then-else`

You make conditionals with the `if` construction.

```
if born_in > current_year then
    write "That looks wrong"
```

```
if current_year = born_in then
    write "You are too young to be using this"
else if current_year < 0 then
    write "What have you done?"
else
    write "You are ", current_year - born_in, " or ", current_year - born_in - 1, "years old"
```

The syntax is

```
if <expression> then <statement>
```

Or

```
if <expression> then <statement> else <statement>
```


## Expressions

Each expression has a data type, the possible data types are `string`, `integer` and `boolean`.

Consist of variables; string characters, numeric and boolean literals; and operators. When accessing a
variable, it must have been already assign; if not, the program will throw an exception and
halt the execution.

Expressions are written in [Polish Notation](https://en.wikipedia.org/wiki/Polish_notation).

### `string` expressions

Consist only of string literals, which are characters enclosed in double-quotes.

```
"This is a string literal"
```

Some special characters can be preceded with a `\`.

- `\t` for tabs
- `\n` for new lines
- `\\` for the `\` character
- `\"` for the `"` character

### `integer` expressions

Consist of integer literals, which are strings of numbers.

```
42
```

The binary operators, which take `integer` for the operands and produce an `integer`.

- `+` for addition
- `-` for substraction
- `*` for multiplication
- `/` for division
- `%` for modulo
- `^` for exponentiation; the right operand must be positive, there are no floating-point numbers

### `boolean` expressions

Consist of the boolean literals.

- `true`
- `false`

The binary operators, which take `boolean` for the operands and produce a `boolean`.

- `or` for boolean disjunction
- `and` for boolean conjunction

And the unary operator, which takes a `boolean` for the operand and produces a `boolean`.

- `not` for negating a boolean

There are also comparation operators between `integer`s, which produce a `boolean`.

- `=` equal to
- `/=` different from
- `>` greater than
- `>=` greater than or equal to
- `<` less than
- `<=` less than or equal to

And comparation operators between `boolean`s, which produce a `boolean`.

- `=` equal to
- `/=` different from
