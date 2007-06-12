open Rep_OclTerm;
open Rep_Core; 
open OclLibrary;
open Context;
open OclLibrary;

val xmi = RepParser.readFile "../examples/company/company.xmi";
val untyped_model = (oclLib@xmi);

val ocl = OclParser.parse_contextlist "../examples/company/company.manuel.ocl";
val pp = Preprocessor.preprocess_context_list ocl xmi;

val c = Literal ("a",Classifier (["Company","Company"]));
val p = Literal ("b",Classifier (["Company","Person"]));
val j = Literal ("c",Classifier (["Company","Job"]));
val b = Literal ("d",Classifier (["Company","Bank"]));

val s = Literal ("s",String);
val i = Literal ("i",Integer);
val r = Literal ("r",Real);
val set = Literal ("set",Set(Integer));
val collection = Literal("collection",Collection(String));
val bag = Literal ("bag",Bag(Real));

val model = (oclLib@xmi);

val cc = get_classifier c model;
val cp = get_classifier p model;
val cj = get_classifier j model;
val cb = get_classifier b model;

