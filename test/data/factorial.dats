fun factorial_recursion {n:nat} .<n>. (n: int(n)) : int =
case+ n of
| 0 => 1
| n =>> factorial_recursion(n-1) * n
