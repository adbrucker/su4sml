(* values *)
val offset = ref 1
val quite = (log_level:= 0)
val verbose = (log_level:= 100)


fun set_log_level x = 
    if (x <= 0) then
	(raise DebuggingError "Debugging level cannot be negative! \n")
    else
	log_level:=x

fun current_offset 0 =  raise DebugOffsetError (" Offset 0 reached \n")
  | current_offset x = 
    if (x = 1) then 
	"\t"
    else
	("\t" ^ (current_offset (x-1)))

fun debug_offset "inc" s x level = 
    let
	val _ = trace high ((current_offset (!offset)) ^ s ^ " : " ^ "\n")
	val _ = if (level = 0) then
		    log_level:= !log_level
		else
		    log_level:= level
    in
	offset := !offset + 1
    end
  | debug_offset "dec" s 0 level = raise DebugOffsetError ("Not possible to decrease offset since it is 0.") 
  | debug_offset "dec" s x level = 
    let
	val _ = trace high ((current_offset (!offset - 1)) ^ "} end " ^ s ^ "\n")
	val _ = if (level = 0) then
		    log_level:= !log_level
		else
		    log_level:= level
    in
	offset := !offset - 1
    end
  | debug_offset s1 s2 x level = raise DebugOffsetError ("Only 'inc' and 'dec' operations possible. \n")
