(** convenience functions for handling strings. *)
structure StringHandling =
struct
open library

(** returns the string in all caps. *)
fun toUpper (s:string) = String.map Char.toUpper s

(** returns the uncapitalized string. *)
fun uncapitalize (s:string) = let val sl = String.explode s
 		              in
 			          String.implode ((Char.toLower (hd sl))::(tl sl))
 		              end
                              
(** returns the capitalized string. *)
fun capitalize (s:string) = let val sl = String.explode s
 		            in
 		                String.implode ((Char.toUpper (hd sl))::(tl sl))
		            end

                            
end
