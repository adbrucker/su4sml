signature WFCPO_NAMING = 
sig
    val get_po_number      : unit -> int
    val reset_po_nr        : unit -> unit
    val reset_count        : unit -> unit
    val generate_name      : string -> string
    val generate_opt_name  : string -> string option -> string
end
structure WFCPO_Naming:WFCPO_NAMING = 
struct 


val po_nr = ref 0 

val count = ref 0

fun get_po_number() = 
    let
	val _ = (po_nr := ((!po_nr) + 1))
    in
	(!po_nr)
    end

fun reset_po_nr() = 
    let
	val _ = po_nr := 0
    in
	print ("po number reseted.\n")
    end

fun reset_count() =
    let
	val _ = count := 0
    in
	print ("count reseted.\n")
    end



fun generate_name s = 
    let
	val _ = count := (!count + 1)
    in
	s ^ Int.toString (!count)
    end

fun generate_opt_name s NONE = 
    let
	val _ = count := (!count + 1)
    in
	generate_name s
    end
  | generate_opt_name s (SOME(x)) = 
    let
	val _ = count := (!count + 1)
    in
	x
    end
end;
