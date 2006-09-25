structure StringHandling =
struct

open StateMachineTypes
(* returns the capitalized string *)
(* string -> string *)
fun toUpper(S:string) = String.implode (List.map Char.toUpper (String.explode S))

(*reassemble given string list by concatenating every time the string character to each element and return the whole as a string *)
(* string list * string -> string *)
fun reassemble(S: string list,c : string) = String.concat (List.map (fn x => x^c) S)
fun concatBefore(S: string list, c: string) = String.concat (List.map (fn x =>c^x) S)

(* Pair list * string -> string *)
fun replace_vars_in_String(L:Pair list,Str) = let val LINES = String.tokens (fn x => x = #"\n") Str
						  fun isVariable(S) = (List.length (List.filter (fn x => x = #"$") (String.explode S))) = 2 (*string->bool*)
						  fun isEqual(s1:string,s2:string) = (s1<=s2) andalso (s1>=s2)(*string*string -> bool*)
						  fun filterOut(VarName) = List.filter (fn X => isEqual(VarN(X),VarName)) L(*string -> Pair list*)
						  fun VarIt(x) = String.tokens (fn x => x = #"$") x
						  fun process([],R) = R
						    | process(h::t,R) = let val Filtered = filterOut(h)
									in
									    if (List.length Filtered) > 0 then process(t,R@[VarV(hd Filtered)])
									    else process(t,R@[h])
									end
						  fun replace(Str) = let val TABBED = String.tokens (fn x => x = #"\t") Str
									 val VARRED = List.map VarIt TABBED
									 val P = List.map (fn X => process(X,[])) VARRED
								     in
									 String.concatWith "\t" (List.map (fn X => (String.concatWith "" X)) P)
								     end
					      in
					      			 String.concatWith "\n" (List.map replace LINES)
					      end

end
