structure HashString = 
struct 
fun hashString _ = ()
end

structure HashTable = 
struct 

		  
datatype ('a, 'b) hash_table = HT of {
	 eq_pred : ('a * 'a) -> bool,
	 not_found : exn,
	 table: (('a*'b) list) ref
}




fun mkTable (_, eq) (_, notFound) = HT{
	    eq_pred = eq,
	    not_found = notFound,
	    table = ref ([])
	  }


fun find    (HT{table,...}) key =
                           Option.map (fn (a,b) => b) ((List.find (fn (x,y) => x =key) (!table))) 
fun insert  (HT{table,...}) (k,v) =  (table:=([(k,v)]@ (!(table))))

end
