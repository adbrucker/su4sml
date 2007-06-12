(* simple script for debugging and testing *)

(* this script requires that the environment variable
    SU4SML_HOME
   points to the su4sml top-level directory (i.e., `pwd`/../su4sml")
*)


(* display settings: printDepth = 1000 *)
Control.Print.printDepth := 5000;

fun print_depth n = 
   (Control.Print.printDepth := n div 2;
       Control.Print.printLength := n); 


val model = ModelImport.import "../examples/ebank/ebank.xmi" 
                               "../examples/ebank/ebank.manuel.ocl" [];

