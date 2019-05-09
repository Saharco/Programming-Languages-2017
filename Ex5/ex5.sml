(*	Sahar Cohen 206824088 saharco@campus.technion.ac.il
	Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il *)

(* Mission 1: *)

datatype ('a,'b) heterolist = nul | && of 'a * ('b,'a) heterolist
infixr 5 &&
fun build4 (x,one,y,two) = x&&one&&y&&two&&nul;
local 
fun unzipHelper1(nul) = [] |
	unzipHelper1(a&&nul) = [a] |
	unzipHelper1(a&&b&&x) = a::unzipHelper1(x);
fun unzipHelper2(nul) = [] |
	unzipHelper2(_&&nul) = [] |
	unzipHelper2(a&&b&&x) = b::unzipHelper2(x);
in
fun unzip(nul) = ([],[]) |
	unzip (a) = (unzipHelper1(a),unzipHelper2(a))
end;
exception Empty;
local
fun zipHelper([],[]) = nul |
	zipHelper(a::[],[]) = a&&nul |
	zipHelper(a::b,c::d) = a&&(c&&(zipHelper(b,d)))
in
fun zip(a,b) = if(length(a)-length(b)=0) then zipHelper(a,b) 
	else if(length(a)-length(b)=1) then zipHelper(a,b) 
	else raise Empty
end;	
	
	
(* Mission 2: *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

exception EmptySeq;

fun head(Cons(x,_)) = x
| head Nil = raise EmptySeq;

fun tail(Cons(_,xf)) = xf()
| tail Nil = raise EmptySeq;

datatype direction = Back | Forward;

datatype 'a bseq =   bNil
| bCons of 'a * (direction -> 'a bseq);

fun bHead(bCons(x,_)) = x
| bHead bNil = raise EmptySeq;

fun bForward(bCons(_,xf)) = xf(Forward)
| bForward bNil = raise EmptySeq;

fun bBack(bCons(_,xf)) = xf(Back)
| bBack bNil = raise EmptySeq;

(* Mission 2.1 *)
fun intbseq num = let
	fun next k Forward = k+1
	| next k dir = k-1
in
bCons(num, fn(dirr) => intbseq(next num dirr))
end;

(* Mission 2.2 *)
fun bmap f bNil = bNil
| bmap f (bCons(x, xfun)) = bCons(f(x), fn(dirr) => bmap f (xfun(dirr)));

(* Mission 2.3 *)
fun bfilter pred d bNil = bNil
| bfilter pred d (bCons(x, xfun)) = if pred x then (bCons(x, fn(dirr) => bfilter pred dirr (xfun(dirr))))
else bfilter pred d (xfun(d));

(* Mission 2.4 *)
local
	fun traverse (Cons(x, xfun)) 0 = x
	| traverse (Cons(x, xfun)) count = traverse (xfun()) (count-1)
	
	fun chooseNext rev_seq for_seq d =
	if d >= 0 then bCons(traverse for_seq d, fn(dirr) =>
		if dirr=Forward then chooseNext rev_seq for_seq (d+1)
		else chooseNext rev_seq for_seq (d-1))
	else bCons(traverse rev_seq ((~1*d)-1), fn(dirr) =>
		if dirr=Forward then chooseNext rev_seq for_seq (d+1)
		else chooseNext rev_seq for_seq (d-1))
in
fun seq2bseq Nil _ = bNil
| seq2bseq _ Nil = bNil
| seq2bseq rev_seq for_seq = chooseNext rev_seq for_seq 0
end; 


(* Mission 2.5 *)
local
	fun countTo (bNil, _, _) = bNil
	| countTo((bCons(x, xfun)), dirr, count) = if count = 0 then (bCons(x, xfun))
	else countTo((xfun(dirr)), dirr, count-1)
in
fun bSeqJump bNil m = bNil
| bSeqJump (bCons(x, xfun)) m = if (m<1) then bNil
	else (bCons(x, fn(dirr) => bSeqJump (countTo((bCons(x, xfun)), dirr, m)) m))
end;
