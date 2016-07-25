signature REP_HELPER =
sig

    (**
     * Filter a list (OBSOLETE?)
     *)
    val filter                  : ('a -> bool) -> 'a list -> 'a list

    (**
     * Return the list without the last element 
     *)
    val  real_path              : 'a list -> 'a list
    
    (**
     * Return the list without the options.
     *)
    val optlist2list            : 'a option list -> 'a list

    (**
     * Remove the duplicate in a given list.
     *)
    val remove_dup              : ''a list -> ''a list

    (**
     * Special version of foldr.
     *)
    val foldr1                  : ('a * 'a -> 'a ) -> 'a list -> 'a

    (**
     * Checks wether a given element is member of a list.
     *)
    val member                  : ''a -> ''a list -> bool

    (**
     * Appends a given list to a list.
     *)
    val append                  : 'a list -> 'a list -> 'a list

    (**
     * Take n elements from a list.
     *)
    val take                    : int * 'a list -> 'a list

    val fst                     : 'a * 'b -> 'a

    val snd                     : 'a * 'b -> 'b

    val join                    : string -> string list -> string

    val |>                      : 'a * ('a -> 'b) -> 'b 

    val curry                   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

    val uncurry                 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

end
structure Rep_Helper:REP_HELPER =
struct 
 
infix |>
fun (x |> f) = f x;


fun filter (pred: 'a->bool) : 'a list -> 'a list =
    let fun filt [] = []
          | filt (x :: xs) = if pred x then x :: filt xs else filt xs
	in filt end;

fun real_path x = List.rev (List.tl (List.rev x))    


fun optlist2list [] = []
  | optlist2list (h::tail) =
    (
     case h  of
	 NONE => optlist2list (tail)
       | SOME (e) => (e::(optlist2list tail))
    )

fun take (n, []) = []
  | take (n, x :: xs) =
    if n > 0 then x :: take (n - 1, xs) else [];

fun foldr1 f l =
    let fun itr [x] = x
          | itr (x::l) = f(x, itr l)
    in  itr l  end;
    
fun exists (pred: 'a -> bool) : 'a list -> bool =
	let fun boolf [] = false
	      | boolf (x :: xs) = pred x orelse boolf xs
	in boolf end;
    
    
fun append xs ys = xs @ ys;
    
fun find _ []        = Option.NONE
  | find p (x :: xs) = if p x then Option.SOME x else find p xs;
    
fun member x [] = false
| member x (h::tail) = 
    if (x = h) then
	true
    else 
	member x tail

fun swap1 f a b c = f c b a


fun fst (x, y) = x
                 
fun snd (x, y) = y


(* fun getenv var =
  (case OS.Process.getEnv var of
      NONE => ""
        | SOME txt => txt);
*)
(*	                                
fun print_depth n = 
   (Control.Print.printDepth := n div 2;
    Control.Print.printLength := n); 
*)

val cd = OS.FileSys.chDir
val pwd = OS.FileSys.getDir

(* use Option.map instead 
fun ap_some f (SOME x) = SOME(f x)
  |ap_some f NONE     = NONE
*)

fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;
(* fun suffix sfx s = s ^ sfx;*)

fun take (n, []) = []
  | take (n, x :: xs) =
    if n > 0 then x :: take (n - 1, xs) else [];
    
fun space_implode a bs = implode (separate a bs);

fun print_stderr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr);

exception ERROR;
   
(** output an informational message about what is going on. *)
fun info s  = print (s^"\n")

(** output a warning that something is wrong, 
 * but it is dealt with somehow.  *)
fun warn s  = print ("Warning: "^s^"\n")

(** output an error message *)
fun error_msg s = print (s^"\n")

(** output an error message and Fail *)
fun error s = (print (s^"\n"); raise Fail s)


fun fst (x, y) = x
                 
fun snd (x, y) = y

fun join s nil = ""
  | join s (h::nil) = h
  | join s (h::t) = h^s^(join s t)

fun uncurry f (x,y) = f x y
fun curry f x y     = f (x,y)

fun remove_dup [] = []
  | remove_dup (h::tail) = if (member h tail) then (remove_dup tail) else ((h)::(remove_dup tail))

fun join s nil = ""
  | join s (h::nil) = h
  | join s (h::t) = h^s^(join s t)

fun uncurry f (x,y) = f x y
fun curry f x y     = f (x,y)


end
