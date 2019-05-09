(*	Sahar Cohen 206824088 saharco@campus.technion.ac.il
	Yuval Nahon 206866832 yuval.nahon@campus.technion.ac.il *)

(* Splay Tree! *)

(* datatype order = LESS | EQUAL | GREATER *)

(* 'a = KEY!; 'b = VALUE! *)
datatype ('a, 'b) SplayTree = 
	Nil
	| SpTr of 'a * 'b * (('a, 'b) SplayTree) * (('a, 'b) SplayTree);

datatype Rotate = Zig | ZigZig | ZigZag;

exception NotFound;

(* Sub 1: *)
fun size Nil = 0
	| size (SpTr(_, _, left, right)) = 1 + size left + size right;

(* Sub 2: *)
local
	(* Helper function that performs the correct rotations according to a given generated sequence *)
	(* Sequence is over: all the rotations have been executed *)
	fun decipher([], _, tree) = tree
	(* Sequence is over: all the rotations have been executed *)
	| decipher(_, [], tree) = tree
	(* We never get here: *)
	| decipher(_, _, Nil) = Nil
	(* ROTATION CASE: zig right*)
	| decipher("R"::[], (SpTr(p_k, p_v, p_left, p_right))::[], (SpTr(key, value, left, right))) =
		(SpTr(key, value, (SpTr(p_k, p_v, p_left, left)) , right))
	(* ROTATION CASE: zig left *)
	| decipher("L"::[], (SpTr(p_k, p_v, p_left, p_right))::[], (SpTr(key, value, left, right))) =
		(SpTr(key, value, left, (SpTr(p_k, p_v, right, p_right))))
	(* ROTATION CASE: zig-zig right *)
	| decipher("R"::"R"::sequence, (SpTr(p_k, p_v, p_left, p_right))::(SpTr(g_k, g_v, g_left, g_right))::path, (SpTr(key, value, left, right))) =
		decipher(sequence, path, (SpTr(key, value, (SpTr(p_k, p_v, (SpTr(g_k, g_v, g_left, p_left)), left)), right)))
	(* ROTATION CASE: zig-zig left *)
	| decipher("L"::"L"::sequence, (SpTr(p_k, p_v, p_left, p_right))::(SpTr(g_k, g_v, g_left, g_right))::path, (SpTr(key, value, left, right))) =
		decipher(sequence, path, (SpTr(key, value, left,(SpTr(p_k, p_v, right,(SpTr(g_k, g_v, p_right, g_right)))))))
	(* ROTATION CASE: zig-zag*)
	| decipher("R"::"L"::sequence, (SpTr(p_k, p_v, p_left, p_right))::(SpTr(g_k, g_v, g_left, g_right))::path, (SpTr(key, value, left, right))) = let
		val new_grand = (SpTr(g_k, g_v, right, g_right))
		val new_parent = (SpTr(p_k, p_v, p_left, left))
	in
		decipher(sequence, path, (SpTr(key, value, new_parent, new_grand)))
	end
	(* ROTATION CASE: zag-zig*)
	| decipher("L"::"R"::sequence, (SpTr(p_k, p_v, p_left, p_right))::(SpTr(g_k, g_v, g_left, g_right))::path, (SpTr(key, value, left, right))) = let
		val new_grand = (SpTr(g_k, g_v, g_left, left))
		val new_parent = (SpTr(p_k, p_v, right, p_right))
	in
		decipher(sequence, path, (SpTr(key, value, new_grand, new_parent)))
	end
	(* Default: we never get here *)
	| decipher(sequence, path, node) = Nil
	
	(* The splay function. Generates the correct sequence of rotations in a list. "R" - right; "L" - left.
	We save the nodes we visited in an additional list, and once the sequence is finished - we decipher it (apply rotations). *)
	fun splay(sequence, path, compare, Nil, key) = Nil
	| splay(sequence, path, compare, (SpTr(k, v, left, right)), key) =
	case compare(key, k) of
		GREATER => splay("R"::sequence, (SpTr(k, v, left, right))::path, compare, right, key)
		| LESS => splay("L"::sequence, (SpTr(k, v, left, right))::path, compare, left, key)
		| EQUAL => decipher(sequence, path, (SpTr(k, v, left, right)))

	fun insert_temp (compare, Nil, (k, v)) = SpTr(k, v, Nil, Nil)
		| insert_temp (compare, (SpTr(curr_key, curr_value, left, right)), (k, v)) =
		case compare(k, curr_key) of
			EQUAL => SpTr (k, v, left, right)
			| GREATER => SpTr (curr_key, curr_value, left, insert_temp(compare, right, (k, v)))
			| LESS => SpTr (curr_key, curr_value, insert_temp(compare, left, (k, v)), right)
in
fun insert compare = fn (tree, (k, v)) => splay([] : string list, [] : ('a, 'b) SplayTree list, compare, insert_temp(compare, tree, (k,v)), k)
end;

(* Sub 3: *)
local 
fun get_temp (compare, Nil, k) = raise NotFound
	| get_temp (compare, (SpTr(curr_key, curr_value, left, right)), k) = 
	case compare(k : 'a, curr_key : 'a) of
		EQUAL => curr_value
		| GREATER => get_temp (compare, right, k)
		| LESS => get_temp (compare, left, k)
in 
fun get compare = fn (tree,k) => get_temp(compare,tree,k)
end;

(* Sub 4: *)
fun inorder Nil = []
	| inorder (SpTr(curr_key, curr_value, left, right)) =
	inorder(left) @ [curr_value] @ inorder(right);

(* Sub 5: *)
local
	fun fullPreorder Nil = []
	| fullPreorder (SpTr(curr_key, curr_value, left, right)) =
	[(curr_key, curr_value)] @ fullPreorder(left) @ fullPreorder(right)
	
	fun listToString [] = ""
	| listToString ((x:int,y:int) :: []) = Int.toString(x) ^ "," ^ Int.toString(y)
	| listToString ((x,y) :: (xs : (int * int) list)) = Int.toString(x) ^ "," ^ Int.toString(y) ^ " " ^ listToString(xs)
	
in
(* The generated code will be one single preorder traversal on the tree (includes the keys and values of all members). *)
fun encode Nil = ""
	| encode tree = 
	listToString(fullPreorder(tree))
end;

(* Sub 6: *)
local
	(* Checks if a letter in the code is valid *)
	fun checkLetter(#" ", code, length, i) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) >= #"0" orelse chr(ord(String.sub(code, i+1))) <= #"9" orelse chr(ord(String.sub(code, i+1))) = #"~")) then true
	else false
	
	| checkLetter(#"~", code, length, i) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) < #"0" orelse chr(ord(String.sub(code, i+1))) > #"9")) then false
	else true
	
	| checkLetter(#",", code, length, i) =
	if(i+1<length andalso (chr(ord(String.sub(code, i+1))) >= #"0" orelse chr(ord(String.sub(code, i+1))) <= #"9" orelse chr(ord(String.sub(code, i+1))) = #"~")) then true
	else false
	
	| checkLetter(letter, code, length, i) =
	if (letter >= #"0" andalso letter <= #"9") then true
	else false
	
	(* Checks whether the entire code is valid or not. Will return true if it is, otherwise will return false *)
	fun checkValid(code, length, i) = if(i >= length) then true
	else if (checkLetter(chr(ord(String.sub(code, i))), code, length, i) = false) then false
	else checkValid(code, length, i+1)
	
	(* Gets the code and an index, and returns the full number that corresponds to the given index *)
	fun makeNumber (code, length, i, result) = if(i >= length) then result
	else if (chr(ord(String.sub(code, i))) = #" " orelse chr(ord(String.sub(code, i))) = #",") then result
	else if (chr(ord(String.sub(code, i))) = #"~") then makeNumber(code, length, i+2, ~((ord(String.sub(code, i+1)) - ord#"0")))
	else if (result < 0) then makeNumber(code, length, i+1, (10*result - (ord(String.sub(code, i)) - ord#"0")))
	else makeNumber(code, length, i+1, (10*result + (ord(String.sub(code, i)) - ord#"0")))
	
	(* Gets the code and an index, and returns the index of the start of the next number in the code *)	
	fun nextIndex (code, length, i) = if(i >= length) then i
	else if(chr(ord(String.sub(code, i))) = #" " orelse chr(ord(String.sub(code, i))) = #",") then i+1
	else nextIndex(code, length, i+1)
	
	(* Turns the given code into a list of two-tuples: (keys, values), according to their original order *)
	fun makeList (code, length, i) = if(i >= length) then []
	else ((makeNumber(code, length, i, 0), makeNumber(code, length, nextIndex(code, length, i), 0)) : (int * int)) :: (makeList(code, length, nextIndex(code, length, nextIndex(code, length, i))) : ((int*int) list))
	
	(* Gets a list of two-tuples (keys, values) from the original tree, and constructs the appropriate tree.
	Stop case: the list is empty. Return an empty tree. 
	Algorithm: for each call, the function also has certain minimum and maximum values. Since the code represents the preorder traversal of a BST -
	we know that if the current key in the list is in between min and max - it has to be the root. We then call the function again recursively, this time
	with the value of the added key replacing either min(left sub-tree) or max(right sub-tree), in order to insert the next key in the correct place. *)
	fun makeTree ([], _, _) = Nil
	| makeTree (((key, value) :: preorder_list) : (int*int) list, min, max) = 
	if (key < min orelse key > max) then makeTree(preorder_list, min, max)
	else SpTr(key, value, makeTree(preorder_list, min, key), makeTree(preorder_list, key, max))
	
in
(* Gets a generated code of a tree (generated from the "encode" function), and constructs the appropriate tree from it.
First, we check if the code is valid. If it isn't - we return the empty tree. Otherwise, we turn the given code into a proper list and feed it to the
makeTree function in order to construct the tree. The default values for min and max are the most extreme cases available for int (as we know, the very
first key in a preorder traversal is always the root - so it has to be bigger than the minimum and smaller than the maximum, no matter the key *)
fun decode "" = Nil
	| decode code =  if(checkValid(code, String.size(code), 0) = false) then Nil
	else makeTree(makeList(code, String.size(code), 0), valOf(Int.minInt), valOf(Int.maxInt))
end;