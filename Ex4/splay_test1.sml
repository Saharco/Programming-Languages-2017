
fun compare_int(num1, num2) = if(num1>num2) then GREATER
else if (num2>num1) then LESS
else EQUAL;

fun compare_char(c1:char, c2:char) = if(c1 > c2) then GREATER
else if (c2 > c1) then LESS
else EQUAL;

(* val tree1 = insert compare_char (Nil, (#"g", 1)); *)

val tree1 = insert compare_int (Nil, (20, 1));
val tree2 = insert compare_int (tree1, (10, 2));
val tree3 = insert compare_int (tree2, (5, 3));
val tree4 = insert compare_int (tree3, (15, 6));
val tree5 = insert compare_int (tree4, (7, 5));
val tree6 = insert compare_int (tree5, (30, 7));
val tree7 = insert compare_int (tree6, (25, 8));
val tree8 = insert compare_int (tree7, (1, 4));
val tree9 = insert compare_int (tree8, (35, 9));
val tree10 = insert compare_int (tree9, (40, 11));
val tree11 = insert compare_int (tree10, (32, 10));
val tree12 = insert compare_int (tree11, (~8, 80));
