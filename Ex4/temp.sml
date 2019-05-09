local
	fun fullPreorder Nil = []
		| fullPreorder (SpTr(curr_key, curr_value, left, right)) =
		[(curr_key, curr_value)] @ fullPreorder(left) @ fullPreorder(right)
	
	fun fullInorder Nil = []
	| fullInorder (SpTr(curr_key, curr_value, left, right)) =
	fullInorder(left) @ [(curr_key, curr_value)] @ fullInorder(right);
in
fun encode Nil = ""
	| encode tree = 
	let
		fun listToString [] = ""
			| listToString ((x:int,y:int) :: []) = Int.toString(x) ^ "," ^ Int.toString(y)
			| listToString ((x,y) :: (xs : (int * int) list)) = Int.toString(x) ^ "," ^ Int.toString(y) ^ " " ^ listToString(xs)
	in
	listToString(fullInorder(tree)) ^ "|" ^ listToString(fullPreorder(tree))
	end
end;





local
	datatype state = FAIL | NEXT | UPDATE_NEXT
	
	fun checkLetter(#" ", code, length, i, result) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) >= #"0" orelse chr(ord(String.sub(code, i+1))) <= #"9" orelse chr(ord(String.sub(code, i+1))) = #"~")) then NEXT
	else FAIL
	
	| checkLetter(#"~", code, length, i, result) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) < #"0" orelse chr(ord(String.sub(code, i+1))) > #"9")) then FAIL
	else NEXT
	
	| checkLetter(#",", code, length, i, result) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) >= #"0" orelse chr(ord(String.sub(code, i+1))) <= #"9" orelse chr(ord(String.sub(code, i+1))) = #"~")) then NEXT
	else FAIL
	
	| checkLetter(#"|", code, length, i, result) =
	if(result <> 0) then FAIL
	else UPDATE_NEXT
	
	| checkLetter(letter, code, length, i, result) =
	if (letter >= #"0" andalso letter <= #"9") then NEXT
	else FAIL
	
	
	fun checkValid(code, length, i, result) =
	if(length = i andalso result <> 0) then result
	else if (length = i andalso result = 0) then ~1
	else case checkLetter(chr(ord(String.sub(code, i))), code, length, i, result) of
		FAIL => ~1
		| NEXT => checkValid(code, length, i+1, result)
		| UPDATE_NEXT => checkValid(code, length, i+1, i+1)
		
	fun makeNumber (code, length, i, result) = if(i >= length) then i
	else if (chr(ord(String.sub(code, i))) = #" " orelse chr(ord(String.sub(code, i))) = #"," orelse chr(ord(String.sub(code, i))) = #"|") then result
	else if (chr(ord(String.sub(code, i))) = #"~") then makeNumber(code, length, i+2, ~((ord(String.sub(code, i+1)) - ord#"0")))
	else if (result < 0) then makeNumber(code, length, i+1, (10*result - (ord(String.sub(code, i)) - ord#"0")))
	else makeNumber(code, length, i+1, (10*result + (ord(String.sub(code, i)) - ord#"0")))
		
	fun nextIndex (code, length, i) = if(i >= length) then i
	else if(chr(ord(String.sub(code, i))) = #" " orelse chr(ord(String.sub(code, i))) = #"," orelse chr(ord(String.sub(code, i))) = #"|") then i
	else nextIndex(code, length, i+1)
	
		
	fun makeList (code, length, i) = if (i >= length orelse chr(ord(String.sub(code, i))) = #"|") then []
	else if (chr(ord(String.sub(code, i))) = #" " orelse chr(ord(String.sub(code, i))) = #",") then makeList(code, length, i+1)
	else ((makeNumber(code, length, i, 0), makeNumber(code, length, 1+nextIndex(code, length, i), 0)) : (int * int)) :: (makeList(code, length, nextIndex(code, length, 1+nextIndex(code, length, i))) : ((int*int) list))
	
	fun splitUntil ([], steps) = []
	| splitUntil (x::xs, steps) = if(steps >= 0) then []
	else x :: splitUntil(xs, steps-1)
	
	fun splitFrom([], steps) = [] 
	| splitFrom (x::xs, steps) = if(steps >= 0) then (x::xs)
	else splitFrom(xs, steps-1)
	
	fun getSteps ([], key, steps) = ~1 
	| getSteps (x::xs, key:(int*int), steps) = if(x = key) then steps
	else getSteps(xs, key, steps+1)
	
	fun makeTree ([], _) = Nil
	| makeTree (_, []) = Nil
	| makeTree (((x1,y1) :: inorder_list : (int*int) list), ((x2,y2) :: preorder_list : (int*int) list)) = 
	let
		val steps = getSteps(((x1,y1)::inorder_list), (x2,y2), 0);
	in
	SpTr(x2, y2, makeTree(splitUntil(((x1,y1)::inorder_list), steps), splitUntil(preorder_list, steps)),
		makeTree(splitFrom(inorder_list, steps), splitFrom(preorder_list, steps)))
	
in
fun decode "" = Nil
	| decode code = 
	let
		val string_size = String.size(code)
		val result = checkValid(code, string_size, 0, 0)
	in
	if(result = ~1) then Nil
	else makeTree(makeList(code, string_size, 0), makeList(code, string_size, result))
	end
end;