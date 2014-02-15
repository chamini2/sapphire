Int x = 10;

----------------------------

Int[3][3] a;
[[Int]] a;
[[Int|3]|3] a;

a = {{1,2,3},{1,2,3},{1,2,3}};

----------------------------

for x in [x..20]
    write "hola"

----------------------------

while (x%13 /= 0) {
    write x;
    x = x+3
}

----------------------------

def plus(a, b) :: Int -> Int -> Int { return a + b }
mas(Int a, Int b) :: Int { return a + b; }

def sum(Int a, Int b) :: Int { return a + b; }
Int sumar(Int a, Int b) { return a + b }


----------------------------

Void nothing() {}
Void nada() { write "hola";;;;;;; write "chao" }
