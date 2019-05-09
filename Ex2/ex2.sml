(* Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il
   Sahar Cohen 206824088 saharco@campus.tchnion.ac.il   *)
   
(* Mission 2: fun perfect *)
local
    fun sum (x,y:int) = if y <= 0 then 0 else if (x mod y = 0) then y+sum(x,y-1) else 0+sum(x,y-1);
in
fun perfect (0:int) = false
	| perfect (1:int) = false
	| perfect (x: int) = (sum(x,x-1) = x)
end;

(* Mission 1: fun dubchar & fun apply_on_nth_char *)
fun dubchar(c:char) = let
val c_string = str(c)
in
c_string ^ c_string
end;


fun apply_on_nth_char (f:char->'a) = fn n:int => fn s:string =>
if (size(s)<n+1 orelse n<0) then f(#"!")
else f(String.sub(s, n));


(* Mission 3: fun balance *)
local
	fun check_c (n:int, #"(":char) = n+1
		| check_c (n:int, #")":char) = n-1
		| check_c (n:int, c:char) = n
in
fun balance(statement:string) = let
		fun check_s(s:string, acc:int, n:int) = 
		if(n = size(s) orelse acc<0)then acc
		else check_s(s, check_c(acc, String.sub(s, n)), n+1)
	in
		if(check_s(statement, 0, 0) = 0) then true
		else false
	end
end;

(* Mission 4: fun sig1,sig2,sig3,sig4,sig5,sig6 *)
fun sig1 x = fn y => fn f => f(x,f(x,y));
fun sig2 (i:int,r:real) = fn f => f(r)^"a"="a";
fun sig3  x = fn y => fn z => fn t => x y z;
fun sig4 x = fn y => fn n:int => fn i:int => 5;
fun sig5 x = fn y => fn z => z(x(y),x(y));
fun sig6 x = fn y=>if x=() then 10 else if y=() then 23 else 0;
