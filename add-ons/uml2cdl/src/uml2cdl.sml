structure Uml2Cdl =
struct

fun uml2cdl input output = ((WriteXmlTree.writeFile output) o 
			    Cdl2Xml.tPackage2Xml o 
			    Xmi2Cdl.readXMI) input

val uml2cdl_stdout = (WriteXmlTree.writeStdOut o 
		      Cdl2Xml.tPackage2Xml o 
		      Xmi2Cdl.readXMI) 
		       
fun main (prog,[a,b]) = uml2cdl a b
  | main (prog,[a])   = uml2cdl_stdout a
  | main (prog,[])    = uml2cdl_stdout "-"
  | main _            = TextIO.output (TextIO.stdOut, "Usage: uml2cdl [INPUT | -] [OUTPUT]\nRead XMI from INPUT (if given), or from stdin (if INPUT=\"-\").\nWrite CDL to OUTPUT (if given), or to stdout.")


(*

val f = _export  "uml2cdl": string * string -> unit;
val _ = f (fn (x,y) => uml2cdl x y )
val g = _import "f": unit -> int;
val main = g 

*)

end

val _ = Uml2Cdl.main(CommandLine.name (), CommandLine.arguments ())
