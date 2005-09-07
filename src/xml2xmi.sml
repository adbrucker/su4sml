(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_parser.sml - an xmi-parser for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     J�rgen Doser <doserj@inf.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)

structure ParseXMI : 
sig
    val readFile: string -> XMI.XmiContent
    (* generic exception if something is wrong *)
    exception IllFormed of string
end =
struct

exception NotYetImplemented
(* generic exception if something is wrong *)
exception IllFormed of string



fun getStringAtt string atts = valOf (XmlTree.attvalue_of string atts)
    handle Option => raise IllFormed ("in getAttValue: did not find attribute "^string)

fun getBoolAtt string atts = 
    let val att = getStringAtt string atts
    in 
	(valOf o Bool.fromString) att 
	handle Option => raise IllFormed ("in getBoolAtt: found attribute "^string^
					  " with unexpected value "^att)
    end


fun getIntAtt string atts = 	  
    let val att = getStringAtt string atts
    in 
	(valOf o Int.fromString) att 
	handle Option => raise IllFormed ("in getIntAtt: found attribute "^string^
					  " with unexpected value "^att)
    end

fun getXmiId    a = getStringAtt "xmi.id"    a
fun getName     a = getStringAtt "name"      a
fun getXmiIdref a = getStringAtt "xmi.idref" a
		 
fun getVisibility atts = 
    let val att = XmlTree.attvalue_of "visibility" atts 
    in
	case att of SOME "public"    => XMI.public
		  | SOME "private"   => XMI.private
		  | SOME "protected" => XMI.protected
		  | SOME "package"   => XMI.package
		  | NONE             => XMI.public
		  | SOME string      => raise IllFormed ("in getVisibility: found unexpected attribute value "^string)
    end

fun getOrdering atts = 
    let val att = getStringAtt "ordering" atts 
    in 
	case att of "unordered" => XMI.Unordered
		  | "ordered"  => XMI.Ordered
		  | _ => raise IllFormed ("in getOrdering: found unexpected attribute value "^att)
    end 

fun getOrderingMaybe atts = 
    let val att = XmlTree.attvalue_of "ordering" atts 
    in 
	case att of SOME "unordered" => XMI.Unordered
		  | SOME "ordered"   => XMI.Ordered
		  | _                => XMI.Unordered
    end 

fun getAggregation atts = 
    let val att = getStringAtt "aggregation" atts in
	case att of "none" => XMI.NoAggregation
		  | "aggregate" => XMI.Aggregate
		  | "composite" => XMI.Composite
		  | _ => raise IllFormed ("in getAggregation: found unexpected attribute value "^att)
    end 

fun getChangeability atts = 
    let val att = getStringAtt "changeability" atts in
	case att of "changeable" => XMI.Changeable
		  | "frozen"     => XMI.Frozen
		  | "addonly"    => XMI.AddOnly
		  | _ => raise IllFormed ("in getChangeability: found unexpected attribute value "^att)
    end 
			       
fun getKind atts = 
    let val att = getStringAtt "kind" atts in
	case att of "in"     => XMI.In
		  | "out"    => XMI.Out
		  | "inout"  => XMI.Inout
		  | "return" => XMI.Return
		  | _ => raise IllFormed ("in getKind: found unexpected attribute value "^att)
    end 
	
fun getRange atts = (getIntAtt "lower" atts, getIntAtt "upper" atts)

fun mkMultiplicity tree = map (getRange o XmlTree.attributes_of) 
			   (((XmlTree.filter "UML:MultiplicityRange") o 
			     (XmlTree.skip   "UML:Multiplicity.range") o hd o 
			     (XmlTree.skip   "UML:Multiplicity")) tree)

fun mkAssociationEnd tree = 
    let fun f atts trees =  
	    { xmiid          = getXmiId atts, 
	      name           = getName atts, 
	      isNavigable    = getBoolAtt "isNavigable" atts,
	      ordering       = getOrdering atts,
	      aggregation    = getAggregation atts,
	      multiplicity   = (mkMultiplicity o hd o (XmlTree.follow "UML:AssociationEnd.multiplicity")) 
							 trees,
	      changeability  = getChangeability atts, 
	      visibility     = getVisibility atts,
	      participant_id = (getXmiIdref o XmlTree.attributes_of o hd o
				(XmlTree.follow "UML:AssociationEnd.participant")) trees }
    in 
	XmlTree.apply_on "UML:AssociationEnd" f tree
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkAssociationEnd: "^msg)
    end

fun mkAssociationClass tree =
    let fun f atts trees = { xmiid      = getXmiId atts, 
			     name       = XmlTree.attvalue_of "name" atts,
			     connection = (map mkAssociationEnd 
					       (XmlTree.follow "UML:Association.connection" 
							       trees)) }
    in 
	XmlTree.apply_on "UML:AssociationClass" f tree
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkAssociation: "^msg)
    end

fun mkAssociation tree = 
    let fun f atts trees = { xmiid      = getXmiId atts, 
			     name       = XmlTree.attvalue_of "name" atts,
			     connection = (map mkAssociationEnd 
					       (XmlTree.skip "UML:Association.connection" 
								 (hd trees))) }
    in 
	XmlTree.apply_on "UML:Association" f tree
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkAssociation: "^msg)
    end

fun mkVariableDec tree = 
    let fun f atts trees = 
	    { xmiid = getXmiId atts,
	      name  = getName atts,
	      declaration_type = (getXmiIdref o XmlTree.attributes_of o hd o 
				  (XmlTree.follow "OCL.Expressions.VariableDeclaration.type")) trees
	      }
    in XmlTree.apply_on "UML15OCL.Expressions.VariableDeclaration" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkVariableDec: "^msg)
    end

	
(* find the xmi.idref attribute of an element pinted to by name *)
fun findXmiIdRef name trees = (getXmiIdref o XmlTree.attributes_of o hd)
				  (XmlTree.follow name trees)

(* find the type of an OCl sub-expression *)
fun findExpressionType trees = findXmiIdRef "OCL.Expressions.OclExpression.type" 
					    trees
					    handle _ => "DummyT"
					    (* hack: return a reference to a dummy*)
					    (* type if the real type is not found *)

(* this is a hack. This will still throw an exception in xmi2mdr, because the  *)
(* expression_type should be the xmiid of oclLib.Boolean, which we do not know *)
val triv_expr = XMI.LiteralExp {symbol = "true", 
				    expression_type = "bool" }

fun mkOCLExpression tree = 
    let val elem  = XmlTree.tagname_of tree
	val atts  = XmlTree.attributes_of tree
	val trees = XmlTree.children_of tree
    in 
	if elem = "UML15OCL.Expressions.BooleanLiteralExp" then
	    XMI.LiteralExp { symbol          = getStringAtt "booleanSymbol" atts,
				 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.IntegerLiteralExp" then
	    XMI.LiteralExp { symbol          = getStringAtt "integerSymbol" atts,
				 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.StringLiteralExp" then
	    XMI.LiteralExp { symbol          = getStringAtt "stringSymbol" atts,
				 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.RealLiteralExp" then
	    XMI.LiteralExp { symbol          = getStringAtt "realSymbol" atts,
				 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.OperationCallExp" then
	    let val op_src = hd (XmlTree.follow 
				      "OCL.Expressions.PropertyCallExp.source"
				      trees)
		val op_ref = 
		    findXmiIdRef 
			"OCL.Expressions.OperationCallExp.referredOperation" trees
		val op_args = XmlTree.follow_all 
				  "OCL.Expressions.OperationCallExp.arguments"
				  trees 
	    in XMI.OperationCallExp 
		   { source            = mkOCLExpression op_src,
		     arguments         = map (mkOCLExpression o hd) op_args,
		     referredOperation = op_ref,
		     expression_type   = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.OclOperationWithTypeArgExp" then
	    let val op_src = hd (XmlTree.follow 
				      "OCL.Expressions.PropertyCallExp.source"
				      trees)
	    in XMI.OperationWithTypeArgExp
	       { source = mkOCLExpression op_src,
		 name = getName atts,
		 typeArgument = findXmiIdRef "OCL.Expressions.OclOperationWithTypeArgExp.typeArgument" trees,
		 expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.AttributeCallExp" then
	    let val att_ref = 
		    findXmiIdRef 
			"OCL.Expressions.AttributeCallExp.referredAttribute" trees
		val att_src = (hd o XmlTree.follow 
					"OCL.Expressions.PropertyCallExp.source") 
				  trees
	    in XMI.AttributeCallExp 
		   { source            = mkOCLExpression att_src,
		     referredAttribute = att_ref,
		     expression_type   = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.AssociationEndCallExp" then
	    let val assoc_src = (hd o XmlTree.follow 
					  "OCL.Expressions.PropertyCallExp.source") 
				    trees
		val assoc_ref = 
		    findXmiIdRef 
			"OCL.Expressions.AssociationEndCallExp.referredAssociationEnd"
			trees
	    in XMI.AssociationEndCallExp 
		   { source = mkOCLExpression assoc_src,
		     referredAssociationEnd  = assoc_ref,
		     expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.AssociationClassCallExp" then
	    raise NotYetImplemented
	else if elem = "UML15OCL.Expressions.VariableExp" then
	    let val var_ref = findXmiIdRef
				  "OCL.Expressions.VariableExp.referredVariable"
				  trees
	    in XMI.VariableExp { referredVariable = var_ref,
			     expression_type  = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.IfExp" then
	    let val cond = (hd o XmlTree.follow 
				     "OCL.Expressions.IfExp.condition") trees
		val then_exp = (hd o XmlTree.follow
					 "OCL.Expressions.IfExp.thenExpression") 
				   trees
		val else_exp = (hd o XmlTree.follow 
					 "OCL.Expressions.IfExp.elseExpression")
				   trees
	    in XMI.IfExp { condition      = mkOCLExpression cond,
		       thenExpression = mkOCLExpression then_exp,
		       elseExpression = mkOCLExpression else_exp,
		       expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.LetExp" then 
	    let val var_decl = (hd o XmlTree.follow 
					 "OCL.Expressions.LetExp.variable") trees
		val var_xmiid    = getXmiId (XmlTree.attributes_of var_decl)
		val var_name     = getName (XmlTree.attributes_of var_decl)
		val var_type_ref = findXmiIdRef 
				       "OCL.Expressions.VariableDeclaration.type" 
				       (XmlTree.children_of var_decl)
		val in_exp = (hd o XmlTree.follow "OCL.Expressions.LetExp.in") trees
		val init_exp = 
		    (hd o XmlTree.follow 
			      "OCL.Expressions.VariableDeclaration.initExpression") 
			(XmlTree.children_of var_decl)
	    in XMI.LetExp 
		   { variable        = { xmiid            = var_xmiid,
					 name             = var_name, 
					 declaration_type = var_type_ref },
		     initExpression  = mkOCLExpression init_exp ,
		     inExpression    = mkOCLExpression in_exp,
		     expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.IterateExp"  then 
	    raise NotYetImplemented
	else if elem = "UML15OCL.Expressions.IteratorExp" then 
	    let val iterator_src = (hd o XmlTree.follow 
					     "OCL.Expressions.PropertyCallExp.source") 
				       trees
		val iterator_body = (hd o XmlTree.follow 
					     "OCL.Expressions.LoopExp.body") 
				       trees
		val iterators = XmlTree.follow "OCL.Expressions.LoopExp.iterators" 
					       trees 
	    in 
		XMI.IteratorExp { name      = getName atts,
				      iterators = map mkVariableDec iterators,
				      body      = mkOCLExpression iterator_body,
				      source    = mkOCLExpression iterator_src,
				      expression_type = findExpressionType trees }
	    end
	else raise IllFormed ("in mkOCLExpression: found unexpected element "^elem)
    end

fun getAssociations t = (map mkAssociation (XmlTree.filter "UML:Association" t))@
			 (map mkAssociationClass (XmlTree.filter "UML:AssociationClass" t))
			
fun filterConstraints  trees = XmlTree.filter "UML:Constraint" trees   
fun filterStereotypes  trees = XmlTree.filter "UML:Stereotype" trees
fun filterVariableDecs trees = XmlTree.filter "UML15OCL.Expressions.VariableDeclaration" trees
fun filterPackages     trees = append (XmlTree.filter "UML:Package" trees)
				      (XmlTree.filter "UML:Model" trees)			      
(* FIX: other classifiers *) 
fun filterClassifiers trees = 
    filter (fn x => let val elem = XmlTree.tagname_of x in
			elem = "UML:Class"                     orelse
			elem = "UML:Primitive"                 orelse
			elem = "UML:DataType"                  orelse
			elem = "UML:Interface"                 orelse
			elem = "UML:Enumeration"               orelse
			elem = "UML15OCL.Types.SequenceType"   orelse
			elem = "UML15OCL.Types.BagType"        orelse
			elem = "UML15OCL.Types.SetType"        orelse
			elem = "UML15OCL.Types.CollectionType" orelse
			elem = "UML15OCL.Types.VoidType"       orelse
			elem = "UML:AssociationClass"
		    end) trees

fun mkConstraint tree = 
    let fun f atts trees =  
	let val expr = (hd o (XmlTree.follow 
				  "OCL.Expressions.ExpressionInOcl.bodyExpression") o
			(XmlTree.follow "UML15OCL.Expressions.ExpressionInOcl") o 
			(XmlTree.follow "UML:Constraint.body"))
			   trees
	    val st_type = hd (XmlTree.follow "UML:ModelElement.stereotype" trees)
	    val st_type_ref  =  getXmiIdref (XmlTree.attributes_of st_type)
	in { xmiid = getXmiId atts,
	     name  = case XmlTree.attvalue_of "name" atts of SOME s => SOME s | _ => NONE,
	     constraint_type = st_type_ref, 
	     body = (mkOCLExpression expr   (* if something goes wrong, we return *)
		     handle _ => triv_expr) (* return trivial expression "true"   *)
	     }
	end
    in XmlTree.apply_on "UML:Constraint" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkConstraint: "^msg)
    end


fun mkParameter tree = 
    let fun f atts trees = { xmiid   = getXmiId atts,
			 name    = getName atts,
			 kind    = getKind atts,
			 type_id = (getXmiIdref o 
				    XmlTree.attributes_of o hd o 
				    (XmlTree.follow "UML:Parameter.type")) 
				       trees }
    in XmlTree.apply_on "UML:Parameter" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkParameter: "^msg)
    end

fun mkOperation tree = 
    let fun f atts trees =
	{ xmiid         = getXmiId atts,
	  name          = getName atts,
	  visibility    = getVisibility atts,
	  isQuery       = getBoolAtt "isQuery" atts,
	  parameter     = (map mkParameter 
			       (XmlTree.follow "UML:BehavioralFeature.parameter" 
					       trees)),
	  constraints   = if XmlTree.exists "UML:ModelElement.constraint" trees
			  then map (getXmiIdref o XmlTree.attributes_of)
				   (XmlTree.follow "UML:ModelElement.constraint" 
						   trees)
			  else nil}
    in XmlTree.apply_on "UML:Operation" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkOperation: "^msg)
    end

fun mkAttribute tree = 
    let fun f atts trees =
	{ xmiid         = getXmiId atts,
	  name          = getName atts,
	  visibility    = getVisibility atts,
	  changeability = getChangeability atts,
	  ordering      = getOrderingMaybe atts, 
	  type_id       = (getXmiIdref o XmlTree.attributes_of o hd o 
			   (XmlTree.follow "UML:StructuralFeature.type")) trees,
	  multiplicity   = (mkMultiplicity o hd o (XmlTree.follow "UML:StructuralFeature.multiplicity"))
							 trees}
    in XmlTree.apply_on "UML:Attribute" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkAttribute: "^msg)
    end

fun mkClass atts trees 
  = XMI.Class { xmiid           = getXmiId atts,
	    name            = getName atts,
	    isActive        = getBoolAtt "isActive" atts,
	    visibility      = getVisibility atts,
	    isLeaf          = getBoolAtt "isLeaf" atts,
	    generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
				  (XmlTree.follow_all 
				       "UML:GeneralizableElement.generalization" 
				       trees)),
	    attributes      = if XmlTree.exists "UML:Classifier.feature" trees
			      then map mkAttribute 
				       ((XmlTree.filter "UML:Attribute") 
					    (XmlTree.follow "UML:Classifier.feature"
							  trees))
			      else nil,
	    operations      = if XmlTree.exists "UML:Classifier.feature" trees
			      then map mkOperation 
				       ((XmlTree.filter "UML:Operation") 
					    (XmlTree.follow "UML:Classifier.feature"
							  trees))
			      else nil,
	    invariant       = if XmlTree.exists "UML:ModelElement.constraint" trees
			      then map (getXmiIdref o XmlTree.attributes_of)
				       (XmlTree.follow "UML:ModelElement.constraint" 
						     trees)
			      else nil}

fun mkPrimitive atts trees 
  = XMI.Primitive { xmiid      = getXmiId atts,
		name       = getName atts,
		operations = if XmlTree.exists "UML:Classifier.feature" trees
			     then map mkOperation 
				      ((XmlTree.filter "UML:Operation") 
					   (XmlTree.follow "UML:Classifier.feature"
							 trees))
			     else nil,
		generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
				       (XmlTree.follow_all 
					    "UML:GeneralizableElement.generalization" 
					    trees)),
		invariant = if XmlTree.exists "UML:ModelElement.constraint" trees
			    then map (getXmiIdref o XmlTree.attributes_of)
				     (XmlTree.follow "UML:ModelElement.constraint" 
						   trees)
			    else nil 
		}
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkPrimitive: "^msg)
    
fun mkEnumeration atts trees 
  = XMI.Enumeration { xmiid      = getXmiId atts,
			  name       = getName atts,
			  operations = if XmlTree.exists "UML:Classifier.feature" trees
				       then map mkOperation 
						((XmlTree.filter "UML:Operation") 
						     (XmlTree.follow "UML:Classifier.feature"
								     trees))
				       else nil,
			  generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
						 (XmlTree.follow_all 
						      "UML:GeneralizableElement.generalization" 
						      trees)),
			  literals = nil, (* FIX *)
			  invariant = if XmlTree.exists "UML:ModelElement.constraint" trees
				      then map (getXmiIdref o XmlTree.attributes_of)
					       (XmlTree.follow "UML:ModelElement.constraint" 
							       trees)
				      else nil
					   }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkEnumeration: "^msg)

fun mkVoid atts trees = XMI.Void { xmiid = getXmiId atts, 
				       name  = getName atts }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkVoid: "^msg)


fun mkGenericCollection atts trees = 
    { xmiid      = getXmiId atts,
      name       = getName atts,
      operations = if XmlTree.exists "UML:Classifier.feature" trees
		   then map mkOperation 
			    ((XmlTree.filter "UML:Operation") 
				 (XmlTree.follow "UML:Classifier.feature"
					       trees))
		   else nil,
      generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
			     (XmlTree.follow_all 
				  "UML:GeneralizableElement.generalization" 
				  trees)),
      elementtype = ((getXmiIdref o XmlTree.attributes_of o hd) 
			 (XmlTree.follow 
			      "OCL.Types.CollectionType.elementType" 
			      trees))
      }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkGenericCollection: "^msg)

				  
fun mkCollection atts trees = XMI.Collection (mkGenericCollection atts trees)
fun mkSequence   atts trees = XMI.Sequence   (mkGenericCollection atts trees)
fun mkSet        atts trees = XMI.Set        (mkGenericCollection atts trees)
fun mkBag        atts trees = XMI.Bag        (mkGenericCollection atts trees)
fun mkOrderedSet atts trees = XMI.OrderedSet (mkGenericCollection atts trees)

fun mkClassifier tree = 
    let val elem  = XmlTree.tagname_of    tree
	val atts  = XmlTree.attributes_of tree
	val trees = XmlTree.children_of    tree
    in 
	if elem = "UML:Class" orelse
	   elem = "UML:AssociationClass" then               mkClass atts trees
	else if elem = "UML:Interface" orelse (* FIX: use a custom mkInterface *) 
		elem = "UML:DataType" orelse
		elem = "UML:Primitive" then              mkPrimitive atts trees
	else if elem = "UML:Enumeration" then         mkEnumeration atts trees
	else if elem = "UML15OCL.Types.VoidType" then        mkVoid atts trees
	else if elem = "UML15OCL.Types.CollectionType" then 
	    mkCollection atts trees
	else if elem = "UML15OCL.Types.SequenceType" then mkSequence atts trees
	else if elem = "UML15OCL.Types.SetType" then           mkSet atts trees
	else if elem = "UML15OCL.Types.BagType" then           mkBag atts trees
	else if elem = "UML15OCL.Types.OrderedSetType" then
	    mkOrderedSet atts trees
	else raise IllFormed ("in mkClassifier: found unexpected element "^elem)
    end


    
fun mkGeneralization tree = 
    let fun f atts trees =
	{ xmiid     = getXmiId atts,
	  child_id  = (getXmiIdref o XmlTree.attributes_of o hd o
		       (XmlTree.follow "UML:Generalization.child")) trees, 
	  parent_id = (getXmiIdref o XmlTree.attributes_of o hd o 
		       (XmlTree.follow "UML:Generalization.parent")) trees }
    in XmlTree.apply_on "UML:Generalization" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkGeneralization: "^msg)
    end


fun mkActivityGraph tree =
    let fun f atts trees = XMI_ActivityGraphs.dummy
    in  XmlTree.apply_on "UML:ActivityGraph" f tree
    end;


fun mkPackage tree = 
    (if XmlTree.tagname_of tree = "UML:Model" orelse
	XmlTree.tagname_of tree = "UML:Package" then 
	 let val trees = XmlTree.skip "UML:Namespace.ownedElement" 
				      ((hd o XmlTree.children_of) tree)
	     val atts = XmlTree.attributes_of tree in
	     XMI.Package { xmiid           = getXmiId atts, 
			       name            = getName atts,
			       visibility      = getVisibility atts,
			       packages        = (map mkPackage 
						      (filterPackages trees)), 
			       classifiers     = (map mkClassifier
						      (filterClassifiers trees)),
			       associations    = getAssociations trees, 
			       generalizations = (map mkGeneralization
						      (XmlTree.filter "UML:Generalization"
								    trees)),
			       constraints     = map mkConstraint 
						     (filterConstraints trees),
                               statemachines   = nil,
                               activitygraphs  = nil
                              }
	 end
     else raise IllFormed "did not find a UML:Model or UML: Package")
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkPackage: "^msg)
				  
				 

fun mkStereotype tree = 
    let fun f atts trees =  { xmiid = getXmiId atts,
			  name = getName atts,
			  baseClass = NONE, (*FIX*)
			  stereotypeConstraint = NONE (*FIX*)
			  }
    in XmlTree.apply_on "UML:Stereotype" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkStereotype: "^msg)
    end 



fun mkXmiContent tree =
    let fun f atts trees = 
	    { packages    = (map mkPackage    (filterPackages trees)),
	      constraints = (map mkConstraint (filterConstraints trees)),
	      classifiers = (map mkClassifier (filterClassifiers trees)),
	      stereotypes = (map mkStereotype (filterStereotypes trees)),
	      variable_declarations = (map mkVariableDec   (filterVariableDecs trees))}
    in XmlTree.apply_on "XMI.content" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkXmiContent: "^msg)
    end
	

val emptyXmiContent = { packages = nil,
			constraints = nil,
			classifiers = nil,
			stereotypes = nil,
			variable_declarations = nil}

fun findXmiContent tree = valOf (XmlTree.dfs "XMI.content" tree)
    handle Option => raise IllFormed "in findXmiContent: did not find XMI.content"
			       
fun readFile f = (mkXmiContent o findXmiContent o ParseXmlTree.readFile) f
    handle XmlTree.IllFormed msg =>  (print ("Warning: "^msg^"\n"); emptyXmiContent)
	 | IllFormed msg => (print ("Warning: "^msg^"\n"); emptyXmiContent)
end


