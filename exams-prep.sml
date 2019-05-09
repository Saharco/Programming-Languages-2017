datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);
fun head (Cons(x, _)) = x;
fun tail (Cons(_, xf)) = xf();
fun mapq f Nil = Nil
	| mapq f (Cons(x, xf)) = Cons(f(x), fn() => mapq f (xf()));
fun filterq pred Nil = Nil
	| filterq pred (Cons(x, xf)) = 
		if pred x then Cons(x, fn() => filterq pred (xf()))
		else filterq pred (xf());
		
fun buildfrom x = (Cons(x, fn()=> buildfrom (x+1))); 

fun buildto x = if (x <= 0) then Nil
else (Cons(x, fn() => buildto (x-1)));

fun incq _ Nil = Nil
| incq compare (Cons(x, xf)) = let
	fun incq_aux max compare Nil = Nil
	| incq_aux max compare (Cons(x, xf)) = 
	if (compare(max, x) = GREATER) then incq_aux max compare (xf())
	else (Cons(x, fn() => incq_aux x compare (xf())))
in
(Cons(x, fn() => incq_aux x compare (xf())))
end;		
		
		
		
		
fun from_with_empty_list k = (Cons((k, []:int list), fn() => from_with_empty_list(k+1)));

fun is_sum_of_list (0, []) = true
	| is_sum_of_list (_, []) = false
	| is_sum_of_list (s, x::xs) = is_sum_of_list(s-x, xs);
	
local
	fun find_dividers (n, count) = if (count >= n) then []
	else if (n mod count = 0) then (find_dividers (n, count+1))@[count]
	else find_dividers (n, count+1)
in
fun dividers Nil = Nil
	| dividers(Cons((x, xl), xf)) = (Cons((x, (find_dividers(x, 1))), fn() => (dividers (xf()))))
end;

fun perfect Nil = Nil
	| perfect seq = mapq (fn(x, xl) => x) (filterq (is_sum_of_list) (dividers (seq)));
	
datatype opr = MULT | DIV
datatype expr = Num of int
				| Var of string
				| BinOp of opr * expr * expr
exception RuntimeError and CompileError

fun count (Num(x)) = 0
	| count (Var(s)) = 0
	| count (BinOp(opr, e1, e2)) = 1 + count(e1) + count(e2);

fun freeVars (Num(x)) = []
	| freeVars (Var(s)) = [s]
	| freeVars (BinOp(opr, e1, e2)) = freeVars(e1) @ freeVars(e2);
	
fun dupcount x 0 = Nil
	| dupcount x k = (Cons((x,k), fn()=>(dupcount x (k-1))));

fun appendq Nil yq = yq
	| appendq (Cons(x, xf)) yq = Cons(x, fn()=> appendq (xf()) yq);

fun dupq f Nil = Nil
	| dupq f (Cons(x, xf)) = appendq (dupcount x (f x)) (Cons((x,0), fn() => dupq f (xf())))

fun insert y 0 xs = y :: xs
	| insert y k (x::xs) = x :: (insert y (k-1) xs);

local
	fun length [] = 0
	| length (x::xs) = 1 + (length xs)
	
	fun permq_aux [] _ = Nil
	| permq_aux (x::xs) n = mapq (fn(p, k) => insert x k p) (dupq length (permq_aux xs (n-1)))
in
	fun permq [] = Nil
	| permq xs = permq_aux xs (length xs)
end;

(*fun subSets [] = [[]]
	| subSets (x::xs) = (subSets xs) @ (map (fn x => (hd x)) (subSets xs));*)
