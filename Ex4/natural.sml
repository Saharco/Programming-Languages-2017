(*	Sahar Cohen 206824088 saharco@campus.technion.ac.il
	Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il *)

datatype natural = zero | Succ of natural;

exception NotNatural;
local
	fun findPrev(n:natural,k:natural) = if(n=Succ(k)) then k else findPrev(n,Succ(k))
	in 
fun prev(zero) = raise NotNatural |
	prev(n:natural) =  findPrev(n,zero)
end;

fun natural_rep(zero) = 0 |
	natural_rep(n:natural) = 1+natural_rep(prev(n)); 



fun group_rep(n:int) = if(n>=0) then (if (n=0) then zero else Succ(group_rep(n-1))) else raise NotNatural;


infix less_eq;
fun (n1:natural) less_eq (n2:natural) = natural_rep(n1) < natural_rep(n2);

infix gadd;
fun (n1:natural) gadd (zero) = n1 |
	(n1:natural) gadd (n2:natural) = Succ(n1 gadd prev(n2)) ;



infix gmul;
fun (n1:natural) gmul (zero) = zero |
	(n1:natural) gmul (n2:natural) = (n1) gadd (n1 gmul prev(n2));


infix gsub;
local 
	fun findSub(n1_original,n1,n2:natural) = if((n1 gadd n2) = n1_original) then n1 else findSub(n1_original,prev(n1),n2)
	in
fun (n1:natural) gsub (n2:natural) = findSub(n1,n1,n2)
end;


infix gdiv;
local
	fun findDiv(n1_original,n1,n2:natural) = if(n1 = n2) then Succ(zero) else( if(n1 less_eq n2) then zero else(Succ(findDiv(n1_original,n1 gsub n2,n2))))
in
fun (n1:natural) gdiv (zero) = raise Div |
	(n1:natural) gdiv (n2:natural) = if(n1=n2) then Succ(zero) else findDiv(n1,n1,n2)
end;

