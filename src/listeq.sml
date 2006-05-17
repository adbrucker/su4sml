(** auxiliary functions on lists of eqtypes.*) 
signature LISTEQ = 
sig
    (** checks whether the list xs includes the value x. *)
    val includes: ''a list -> ''a      -> bool
    
    (** checks whether the intersection of xs and ys is nonempty. *)
    val overlaps: ''a list -> ''a list -> bool

    (** checks whether the lists are disjunct, i.e., do not overlap. *)
    val disjunct: ''a list -> ''a list -> bool
end


structure ListEq:LISTEQ =
struct
open List

(** checks whether the list xs includes the value x. *)
fun includes xs x = exists (fn y => y=x) xs 

(** checks whether the intersection of xs and ys is nonempty. *)
fun overlaps xs ys = includes (map (includes xs) ys) true

(** checks whether the lists are disjunct, i.e., do not overlap. *)
fun disjunct xs ys = not (overlaps xs ys)

end
