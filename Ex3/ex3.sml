(*	Sahar Cohen 206824088 saharco@campus.technion.ac.il
	Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il *)

(* Mission 2.1 : *)

(* (1) Transpose: *)
fun T ([] :: _) = []
| T rows : (real list list) = (map hd rows) :: T (map tl rows);

(* (3) Addition *)
infix ++;
local
	fun sumRow([], []) = []
	| sumRow (x :: xs : (real list), y :: ys : (real list)) = (x + y) :: sumRow(xs, ys)
in
	fun [] ++ [] = []
	| (row1 :: rows1) ++ (row2 :: rows2) = sumRow(row1, row2) :: (rows1 ++ rows2)
end;


(* (4) Multiplication *)
infix **;
local
	fun scalarMulti([], []) = 0.0
	| scalarMulti(x :: xs, y :: ys) = x*y + scalarMulti(xs, ys)
	fun rowProd(row, []) = []
	| rowProd(row, col :: cols) = scalarMulti(row, col) :: rowProd(row, cols)
	fun rowListProd([], cols) = []
	| rowListProd(row :: rows, cols) = rowProd(row, cols) :: rowListProd(rows, cols)
in
	fun rows1 ** rows2 = rowListProd(rows1, T rows2)
end;


(* (2) Invert *)
local
	(* Calculates the size of a square matrix (number of rows) *)
	fun size([]) = 0
	| size(x::xs) = 1 + size(xs)

	(* Also calculates the matrix's size, but returned value is of type real *)
	fun real_size([]) = 0.0
	| real_size(x::xs) = 1.0 + real_size(xs)
	
	(* Returns x[i] in list x *)
	fun rowGetValue(x, 1) = hd x
	| rowGetValue(x, i) = rowGetValue(tl x, i-1)
	
	(* Returns m[i][j] in matrix m *)
	fun matrixGetValue(m, i, j) = rowGetValue(rowGetValue(m, i), j)
	
	fun createIdentityRow(i, row, id_size) = if (i = id_size) then []
		else if (i = row) then 1.0 :: createIdentityRow(i+1, row, id_size)
		else 0.0 :: createIdentityRow(i+1, row, id_size)
	
	fun createIdentityHelper(row, id_size) = if(row = id_size) then []
	else createIdentityRow(0, row, id_size) :: createIdentityHelper(row+1, id_size)
	
	(* Creates the identity matrix of a desired size *)
	fun createIdentity(id_size) = if(id_size < 1) then []
	else createIdentityHelper(0, id_size)

	fun joinMatrices(m1, m2, 0) = []
	| joinMatrices(m1, m2, row) = (hd m1 @ hd m2) :: joinMatrices(tl m1, tl m2, row - 1)

	(* Combines two n x n matrices into one n x 2n matrix:
	the first n columns are those of matrix m1, and the rest are from m2 *)
	fun concatMatrices(m1, m2) = joinMatrices(m1, m2, size m1)
	
	fun removeRow(row, num, i) = if i<=num/2.0 then removeRow(tl row, num, i+1.0)
	else row

	fun separateMatrix([], num) = []
	| separateMatrix(m, num) = removeRow(hd m, num, 1.0) :: separateMatrix(tl m, num)

	(* Gets a n x 2n matrix, and returns the second half's n x n matrix (rows n+1 to 2n) *)
	fun splitMatrix m = separateMatrix(m, real_size(hd(m)))
	
	(* Gets a list, a number and an index.
	Overrides the element in the list corresponding to the given index with the given number *)
	fun overrideRow(row, x, 1) = x::tl(row)
	| overrideRow(row, x, i) = hd(row)::overrideRow(tl(row), x, i-1)
	
	(* Gets a matrix and two indexes indicating its rows, and swaps them *)
	fun matrixSwapRows(m, row1, row2) = overrideRow(overrideRow(m, rowGetValue(m, row1), row2), rowGetValue(m, row2), row1)

	(* Special case: the row in the matrix below the current row already has a 0 as a leading number.
	We'll find the first row after it that has a non-zero element *)
	fun findToSwap(m,i,j) = if Math.pow(matrixGetValue(m,j,i), 2.0) > 0.0 then matrixSwapRows(m,i,j)  
	else findToSwap(m,i,j+1)
	
	(* Gets two lists and substracts them *)
	fun subtractRows([], []) = []
	| subtractRows(row1 : (real list), row2 : (real list)) = (hd row1 - hd row2) :: subtractRows(tl row1, tl row2)
	
	(* Gets a list and a number, and multiplies all of the list's elements by the given number *)
	fun rowMulti(num : real, []) = []
	| rowMulti(num, row) = (hd(row) * num) :: rowMulti(num, tl row)
	
	(* gaussianRows and gaussianCols perform the "gaussian elimination" process on the matrix.
	The former is responsible for handling all of the rows, and the latter iteratres over the columns.
	At the end of this process - we will get the identity matrix on the left side of the matrix,
	and the inverse matrix of the input matrix on the right side of the matrix *)
	fun gaussianRows(m, row, i, k) = if k > row then m
	else if k = i then gaussianRows(m, row, i, k+1)
	else gaussianRows(overrideRow(m, subtractRows(rowGetValue(m, k), rowMulti(matrixGetValue(m, k, i), rowGetValue(m, i))), k), row, i, k+1)
   
	fun gaussianCols(m, col, i) = if  i > col then m
	else if Math.pow(matrixGetValue(m, i, i), 2.0) > 0.0 then gaussianCols(gaussianRows(overrideRow(m, rowMulti((1.0/matrixGetValue(m, i, i)), rowGetValue(m, i)), i), col, i, 1), col, i+1)
	else gaussianCols(findToSwap(m, i, i), col, i)
	
in
	(* The desired function. We concat the given matrix with the identity matrix of the same size, and then
	we proceed to perform the gaussian elimination process on the combined matrix. After we're done - 
	we will return the second half of the matrix (split it back into two) *)
	fun invertible m = splitMatrix(gaussianCols(concatMatrices(m, createIdentity(size(m))), size(m), 1))
end;


(* Mission 2.2 : *)

local
fun min (x,[]) = [] 
| min(x,y::ys) = (if (x<y) then x else y)::min(x,ys)
in
fun max_city (west::[]) (north::northS) = min(west,north::northS)::[]
| 	max_city (west::westS) (north::northS) = (min(west,north::northS)::[]) @ (max_city (westS) (north::northS));
end;


local
fun T ([] :: _) = []
| T rows : (int list list) = (map hd rows) :: T (map tl rows);

fun delete (x:int,[]) = [] |
	delete (x:int,y::ys) = if(x=y) then 0::ys else y::ys;
fun zero ([]) = [] |
	zero (y::ys) = 0::zero(ys);
fun findMatch (x,[]) = [] |	
	findMatch (x,y::ys) = if (x=y) then y::zero(ys) else 0::findMatch(x,ys);

fun checkZero ([]) = true |
	checkZero (l::ls) = if (l>0) then false else checkZero ls;
fun checkSmaller(_,[],_) = [] |
	checkSmaller(_,_,[]) = [] |
	checkSmaller(x,y::ys,l::ls) = if(checkZero(l::ls)) then (if (x<y) then  x::ls else l::checkSmaller(x,ys,ls)) else l::ls;


fun min_city_first_step ([],_) = [] | 
	min_city_first_step(_,[]) = [] |
	min_city_first_step (west::westS,north::northS) =  checkSmaller(west,north::northS,findMatch(west,north::northS))::(min_city_first_step (westS,delete(west,north::northS)));

fun check_number (x,[]) = true |
	check_number (x,l::ls) = if(x=l) then false else check_number(x,ls);


fun second_step(_,[],[]) = [] |
	second_step(north,l::ls,west::westS) = if(check_number(north,l::ls)) then (if (north<=west) then north::ls else l::second_step(north,ls,westS)) else l::ls;   



fun calculate_after_transpose ([],_,_) = [] |
	calculate_after_transpose(_,[],_) = [] |
	calculate_after_transpose(_,_,[]) = [] |
	calculate_after_transpose(matrix::matrixS,west::westS,north::northS) = second_step(north,matrix,west::westS)::calculate_after_transpose(matrixS,west::westS,northS);

in
fun min_city ([]) _ = [] |
	min_city _ ([]) = [] |
	min_city (west::westS) (north::northS) = T(calculate_after_transpose(T(min_city_first_step (west::westS,north::northS)),west::westS,north::northS));
end;