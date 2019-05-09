(*Tests*)

fun help ch = perfect (ord ch);

fun question1 () = if dubchar (#"$") <> "$$" then "Failed in 1'1"  (*1'1*)
                   else if dubchar (#"A") <> "AA" then "Failed in 1'2" (*1'2*)

                   else if apply_on_nth_char ord 2 "ABC" <> ord #"C" then "Failed in 1'3" (*1'3*)
                   else if apply_on_nth_char ord ~1 "ABC" <> ord #"!" then "Failed in 1'4" (*1'4*)
                   else if apply_on_nth_char ord 3 "ABC" <> ord #"!" then "Failed in 1'5" (*1'5*)
                   else if apply_on_nth_char ord 0 "" <> ord #"!" then "Failed in 1'6" (*1'6*)
                   else if apply_on_nth_char ord ~1 "" <> ord #"!" then "Failed in 1'7" (*1'7*)
                   else if apply_on_nth_char dubchar 7 "ABC" <> "!!" then "Failed in 1'8" (*1'8*)
                   else if apply_on_nth_char dubchar 1 "ABC" <> "BB" then "Failed in 1'9" (*1'9*)
                   else if apply_on_nth_char dubchar 0 "ABC" <> "AA" then "Failed in 1'10" (*1'10*)
                   else if apply_on_nth_char dubchar 3 "ABC" <> "!!" then "Failed in 1'11" (*1'11*)
                   else if apply_on_nth_char String.str 2 "!!!!" <> "!"  then "Failed in 1'12" (*1'12*)
                   else if apply_on_nth_char String.str 2 "ABCDE" <> "C"  then "Failed in 1'13" (*1'13*)
                   else if apply_on_nth_char String.str 0 "ABCDE" <> "A"  then "Failed in 1'14" (*1'14*)
                   else if apply_on_nth_char String.str 5 "ABCDE" <> "!"  then "Failed in 1'15" (*1'15*)
                   else if apply_on_nth_char help 2 "ABC" <> false then "Failed in 1'16" (*1'16*)

                   else "Passed Question 1";


fun question2 () = if perfect 6 <> true then "Failed in 2'1" (*2'1*)
				   else if perfect 496 <> true then "Failed in 2'2" (*2'2*)
				   else if perfect 497 <> false then "Failed in 2'3" (*2'3*)
				   else if perfect 8130 <> false then "Failed in 2'4" (*2'4*)
				   else if perfect 0 <> false then "Failed in 2'5" (*2'5*)
				   else if perfect 28 <> true then "Failed in 2'6" (*2'6*)
				   else if perfect 8128 <> true then "Failed in 2'7" (*2'7*)
				   else if perfect 5 <> false then "Failed in 2'8" (*2'8*)
				   else if perfect ~28 <> false then "Failed in 2'9" (*2'9*)
				   else if perfect ~1 <> false then "Failed in 2'10" (*2'10*)
				   else if perfect 1 <> false then "Failed in 2'11" (*2'11*)
				   else if perfect 33550336 <> true then "Failed in 2'12" (*2'12*)

				   else "Passed Question 2";


fun question3 () = if balance "()" <> true then "Failed in 3'1" (*3'1*)
				   else if balance "if(true) then (foo(5))" <> true then "Failed in 3'2" (*3'2*)
				   else if balance ":(" <> false then "Failed in 3'3" (*3'3*)
				   else if balance ")(" <> false then "Failed in 3'4" (*3'4*)
				   else if balance "" <> true then "Failed in 3'5" (*3'5*)
				   else if balance "(" <> false then "Failed in 3'6" (*3'6*)
				   else if balance ")" <> false then "Failed in 3'7" (*3'7*)
				   else if balance "((()aaacc)a)a)cd(" <> false then "Failed in 3'8" (*3'8*)
				   else if balance "()(())acdA" <> true then "Failed in 3'9" (*3'9*)
				   else if balance "()()())(()" <> false then "Failed in 3'10" (*3'10*)
				   else if balance "(()()()())" <> true then "Failed in 3'11" (*3'11*)
				   else if balance "(((())))" <> true then "Failed in 3'12" (*3'12*)
				   else if balance "(()((())()))" <> true then "Failed in 3'13" (*3'13*)
				   else if balance "((((((())" <> false then "Failed in 3'14" (*3'14*)
				   else if balance "()))" <> false then "Failed in 3'15" (*3'15*)
				   else if balance "(()()(()" <> false then "Failed in 3'16" (*3'16*)

				   else "Passed Question 3";


fun check () = if question1() <> "Passed Question 1" then question1()
			   else if question2() <> "Passed Question 2" then question2()
			   else if question3() <> "Passed Question 3" then question3()
			   else "Passed All Tests :)";


check();