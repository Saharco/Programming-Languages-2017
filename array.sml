(* Will throw an exception if the key does not exist in the array *)
exception NotFound;

(* Returns an empty array *)
val newArray = fn(x) => raise NotFound;

(* Adds an element to the array: returns the new array *)
fun addElem array (key, value) = fn(x) => (if x = key then value else (array x));

(* Combines two arrays into one: returns it *)
fun combine array1 array2 = fn(x) => (array1 x) handle NotFound => (array2 x);

(* Checks if a given key exists in the array *)
fun exists array key = ((array key) = (array key)) handle NotFound => false;