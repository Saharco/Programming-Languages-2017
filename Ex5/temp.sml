local
	fun bSeqJumpAux ((bCons(x, xfun)), count) =
	if count = 0 then x
	else bSeqJumpAux ((bCons(xfun(dirr), xfun)), count-1)
in
fun bSeqJump bNil m = bNil
| bSeqJump (bCons(x, xfun)) m = bSeqJump(bSeqJumpAux, m)
end;