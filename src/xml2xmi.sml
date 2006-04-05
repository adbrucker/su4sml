(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_parser.sml - an xmi-parser for the import interface for su4sml
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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
open library
exception NotYetImplemented
(* generic exception if something is wrong *)
exception IllFormed of string
exception OCLIllFormed of string



fun getStringAtt string atts = valOf (XmlTree.attvalue_of string atts)
    handle Option => raise IllFormed ("in getStringAtt: did not find attribute "^string)

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

val getLang      = getStringAtt "language" 
val getBody      = getStringAtt "body" 
val getXmiId     = getStringAtt "xmi.id"    
val getName      = getStringAtt "name"      
val getXmiIdref  = getStringAtt "xmi.idref" 

fun getMaybeEmptyName atts = Option.getOpt(XmlTree.attvalue_of "name" atts,"")
		 
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

fun getTargetScope atts = 
    let val att = getStringAtt "targetScope" atts 
    in 
	case att of "instance" => XMI.InstanceScope
		  | "classifier"  => XMI.ClassifierScope
		  | _ => raise IllFormed ("in getTargetScope: found unexpected attribute value "^att)
    end 

fun getTargetScopeMaybe atts = 
    let val att = XmlTree.attvalue_of "targetScope" atts 
    in 
	case att of SOME "instance"   => XMI.InstanceScope
		  | SOME "classifier" => XMI.ClassifierScope
		  | NONE              => XMI.InstanceScope
		  | SOME s => raise IllFormed ("in getTargetScope: found unexpected attribute value "^s)
    end 

fun getOwnerScopeMaybe atts = 
    let val att = XmlTree.attvalue_of "ownerScope" atts 
    in 
	case att of SOME "instance" => XMI.InstanceScope
		  | SOME "classifier"  => XMI.ClassifierScope
		  | SOME s => raise IllFormed ("in getOwnerScope: found unexpected attribute value "^s)
		  | NONE => XMI.InstanceScope
    end 

fun getOrderingMaybe atts = 
    let val att = XmlTree.attvalue_of "ordering" atts 
    in 
	case att of SOME "unordered" => XMI.Unordered
		  | SOME "ordered"   => XMI.Ordered
		  | NONE             => XMI.Unordered
		  | SOME s => raise IllFormed ("in getOrdering: found unexpected attribute value "^s)
    end 

fun getAggregation atts = 
    let val att = getStringAtt "aggregation" atts in
	case att of "none" => XMI.NoAggregation
		  | "aggregate" => XMI.Aggregate
		  | "composite" => XMI.Composite
		  | _ => raise IllFormed ("in getAggregation: found unexpected attribute value "^att)
    end 

fun getAggregationMaybe atts = 
    let val att = XmlTree.attvalue_of "aggregation" atts in
	case att of SOME "none" => XMI.NoAggregation
		  | SOME "aggregate" => XMI.Aggregate
		  | SOME "composite" => XMI.Composite
		  | NONE             => XMI.NoAggregation
		  | SOME x => raise IllFormed ("in getAggregation: found unexpected attribute value "^x)
    end 

fun getChangeability atts = 
    let val att = getStringAtt "changeability" atts in
	case att of "changeable" => XMI.Changeable
		  | "frozen"     => XMI.Frozen
		  | "addonly"    => XMI.AddOnly
		  | _ => raise IllFormed ("in getChangeability: found unexpected attribute value "^att)
    end 

fun getChangeabilityMaybe atts = 
    let val att = XmlTree.attvalue_of "changeability" atts in
	case att of SOME "changeable" => XMI.Changeable
		  | SOME "frozen"     => XMI.Frozen
		  | SOME "addonly"    => XMI.AddOnly
		  | NONE              => XMI.Changeable
		  | SOME x =>  raise IllFormed ("in getChangeability: found unexpected attribute value "^x)
    end 
			       
fun getKind atts = 
    let val att = getStringAtt "kind" atts in
	case att of "in"     => XMI.In
		  | "out"    => XMI.Out
		  | "inout"  => XMI.Inout
		  | "return" => XMI.Return
		  | _ => raise IllFormed ("in getKind: found unexpected attribute value "^att)
    end 

fun getPseudoStateKindAttr atts = 
    let val att = getStringAtt "kind" atts 
    in 	(case att of "initial"  => XMI.initial
		   | "deep"     => XMI.deep
		   | "shallow"  => XMI.shallow
		   | "join"     => XMI.join
		   | "fork"     => XMI.fork
		   | "junction" => XMI.junction
		   | "choice"   => XMI.choice
		   | _          => raise IllFormed ("in getPseudoStateKind: found unexpected attribute value "^att))
    end

	
fun getRange atts = (getIntAtt "lower" atts, getIntAtt "upper" atts)

fun mkMultiplicity tree = map (getRange o XmlTree.attributes_of) 
			   (((XmlTree.filter "UML:MultiplicityRange") o 
			     (XmlTree.skip   "UML:Multiplicity.range") o hd o 
			     (XmlTree.skip   "UML:Multiplicity")) tree)
			   handle Empty => raise IllFormed ("Empty in mkMultiplicity")

fun mkAssociationEnd tree = 
    let fun f atts trees =  
	    { xmiid          = getXmiId atts, 
	      name           = XmlTree.attvalue_of "name" atts, 
	      isNavigable    = getBoolAtt "isNavigable" atts,
	      ordering       = getOrderingMaybe atts,
	      aggregation    = getAggregationMaybe atts,
	      targetScope    = getTargetScopeMaybe atts,
	      multiplicity   = if XmlTree.exists "UML:AssociationEnd.multiplicity" trees 
			       then (mkMultiplicity o hd o (XmlTree.follow "UML:AssociationEnd.multiplicity")) 
					trees
			       else [(0,~1)],
	      changeability  = getChangeabilityMaybe atts, 
	      visibility     = getVisibility atts,
	      participant_id = (getXmiIdref o XmlTree.attributes_of o hd o
				(XmlTree.follow "UML:AssociationEnd.participant")) trees }
    in 
	XmlTree.apply_on "UML:AssociationEnd" f tree
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkAssociationEnd: "^msg)
	     | Empty => raise IllFormed ("Empty in mkAssociationEnd")
	     | _ => raise IllFormed ("Error in mkAssociationEnd")

    end


(* FIX: this is a hack to handle AssociationClasses like Associations. *)
(* It neglects the participation ot the AssociationClass itself in the *)
(* Association. It only handles the association between the connected  *)
(* classes.                                                            *)
fun mkAssociationFromAssociationClass tree =
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
					       (XmlTree.follow "UML:Association.connection" 
								  trees)) }
    in 
	XmlTree.apply_on "UML:Association" f tree
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkAssociation: "^msg)
	     | _ => raise IllFormed ("Error in mkAssociation")
    end

(* find the xmi.idref attribute of an element pointed to by name *)
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

(* FIX: this is only a dummy implementation *)
fun mkCollectionLiteralPart x = (getXmiIdref (XmlTree.attributes_of x))

fun mkOCLExpression tree = 
    let val elem  = XmlTree.tagname_of tree
	val atts  = XmlTree.attributes_of tree
	val trees = XmlTree.node_children_of tree
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
	else if elem = "UML15OCL.Expressions.CollectionLiteralExp" then
	    XMI.CollectionLiteralExp { parts = nil, (* map mkCollectionLiteralPart (XmlTree.follow "OCL.Expressions.CollectionLiteralExp.parts" trees), *)
				      expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.OperationCallExp" then
	    let val op_src = hd (XmlTree.follow 
				      "OCL.Expressions.PropertyCallExp.source"
				      trees)
		val op_ref = 
		    findXmiIdRef 
			"OCL.Expressions.OperationCallExp.referredOperation" trees
		val op_args = XmlTree.follow 
				  "OCL.Expressions.OperationCallExp.arguments"
				  trees 
	    in XMI.OperationCallExp 
		   { source            = mkOCLExpression op_src,
		     arguments         = map mkOCLExpression op_args,
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
				       (XmlTree.node_children_of var_decl)
		val in_exp = (hd o XmlTree.follow "OCL.Expressions.LetExp.in") trees
		val init_exp = 
		    (hd o XmlTree.follow 
			      "OCL.Expressions.VariableDeclaration.initExpression") 
			(XmlTree.node_children_of var_decl)
	    in XMI.LetExp 
		   { variable        = { xmiid            = var_xmiid,
					 name             = var_name, 
					 declaration_type = var_type_ref,
					 init = SOME (mkOCLExpression init_exp)},
		     inExpression    = mkOCLExpression in_exp,
		     expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.IterateExp"  then 
	    let val iterator_src = (hd o XmlTree.follow 
					     "OCL.Expressions.PropertyCallExp.source") 
				       trees
		val iterator_body = (hd o XmlTree.follow 
					     "OCL.Expressions.LoopExp.body") 
				       trees
		val iterators = XmlTree.follow "OCL.Expressions.LoopExp.iterators" 
					       trees 
		val iterate_result = (hd o XmlTree.follow "OCL.Expressions.IterateExp.result") 
						    trees
	    in 
		XMI.IterateExp { result = mkVariableDec iterate_result,
				  iterators = map mkVariableDec iterators,
				  body      = mkOCLExpression iterator_body,
				  source    = mkOCLExpression iterator_src,
				  expression_type = findExpressionType trees }
	    end
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
and mkVariableDec vtree = 
    let fun f atts trees = 
	    { xmiid = getXmiId atts,
	      name  = getName atts,
	      init = Option.map (mkOCLExpression o hd o XmlTree.node_children_of)
				(XmlTree.find_some "OCL.Expressions.VariableDeclaration.initExpression" trees),
	      declaration_type = (getXmiIdref o XmlTree.attributes_of o hd o 
				  (XmlTree.follow "OCL.Expressions.VariableDeclaration.type")) trees
	      }
    in XmlTree.apply_on "UML15OCL.Expressions.VariableDeclaration" f vtree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkVariableDec: "^msg)
    end

	

fun getAssociations t = (map mkAssociation (XmlTree.filter "UML:Association" t))@
			 (map mkAssociationFromAssociationClass 
			      (XmlTree.filter "UML:AssociationClass" t))
			handle _ => raise IllFormed ("Error in getAssociations") 

val filterConstraints   = XmlTree.filter "UML:Constraint"  
val filterStereotypes   = XmlTree.filter "UML:Stereotype" 
val filterVariableDecs  = XmlTree.filter "UML15OCL.Expressions.VariableDeclaration" 
val filterPackages      = fn trees => append (XmlTree.filter "UML:Package" trees)
				             (XmlTree.filter "UML:Model" trees)			      
val filterStateMachines = XmlTree.filter "UML:StateMachine" 
val filterActivityGraphs= XmlTree.filter "UML:ActivityGraph" 
val filterEvents        = XmlTree.filter "UML:CallEvent" (* add SignalEvents? *)

(* there may be other kinds of dependencies, but we do not parse them atm *)
val filterDependencies = XmlTree.filter "UML:Abstraction" 

val filterTagDefinitions = XmlTree.filter "UML:TagDefinition" 

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

fun mkDependency tree =
    let fun f atts trees =
	    { xmiid    = getXmiId atts,
	      client   = ((getXmiIdref o XmlTree.attributes_of o hd) 
			      (XmlTree.follow "UML:Dependency.client" trees)),
	      supplier = ((getXmiIdref o XmlTree.attributes_of o hd) 
			      (XmlTree.follow "UML:Dependency.supplier" trees)),
	      stereotype = ((getXmiIdref o XmlTree.attributes_of o hd) 
				(XmlTree.follow "UML:ModelElement.stereotype" trees))}
    in XmlTree.apply_on "UML:Abstraction" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkDependency: "^msg) 
    end

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
	     body = (mkOCLExpression expr) 
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
	    | _ => raise IllFormed ("Error in mkParameter")
    end

fun mkOperation tree = 
    let fun f atts trees =
	{ xmiid         = getXmiId atts,
	  name          = getName atts, 
	  visibility    = getVisibility atts,
	  isQuery       = getBoolAtt "isQuery" atts,
	  ownerScope    = getOwnerScopeMaybe atts,
	  parameter     = (map mkParameter 
			       (XmlTree.follow "UML:BehavioralFeature.parameter" 
					       trees)),
	  constraints   =  map (getXmiIdref o XmlTree.attributes_of)
			       (XmlTree.follow "UML:ModelElement.constraint" 
					       trees)
			       }
    in XmlTree.apply_on "UML:Operation" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkOperation: "^msg)
	    | IllFormed msg =>  raise IllFormed ("in mkOperation: "^msg)
	    | _ => raise IllFormed ("Error in mkOperation")

    end


fun mkTaggedValue tree =
    let fun f atts trees ={xmiid    = getXmiId atts,
                           dataValue= (String.concat 
					   (map XmlTree.text_of 
						(XmlTree.children_of 
						     (XmlTree.find "UML:TaggedValue.dataValue" trees)))),
                           tag_type = (getXmiIdref o XmlTree.attributes_of o 
                                      (XmlTree.find "UML:TagDefinition") o
                                      XmlTree.node_children_of o  
                                      (XmlTree.find "UML:TaggedValue.type")) trees
			   }
    in  XmlTree.apply_on "UML:TaggedValue" f tree 
	handle XmlTree.IllFormed msg => raise IllFormed ("in mkTaggedValue: "^msg)
    end

fun mkAttribute tree = 
    let fun f atts trees =
	{ xmiid         = getXmiId atts,
	  name          = getName atts,
	  visibility    = getVisibility atts,
	  changeability = getChangeabilityMaybe atts,
	  ordering      = getOrderingMaybe atts, 
	  initialValue  = Option.map (mkOCLExpression o hd o		       
				      (XmlTree.follow 
					   "OCL.Expressions.ExpressionInOcl.bodyExpression") o
				      (XmlTree.follow "UML15OCL.Expressions.ExpressionInOcl") o 
				       XmlTree.node_children_of) 
				     (XmlTree.find_some "UML:Attribute.initialValue" trees)
	                   handle _ => NONE,
	  type_id       = (getXmiIdref o XmlTree.attributes_of o hd o 
			   (XmlTree.follow "UML:StructuralFeature.type")) trees,
	  multiplicity   = if XmlTree.exists "UML:StructuralFeature.multiplicity" trees 
			   then (mkMultiplicity o hd o (XmlTree.follow "UML:StructuralFeature.multiplicity")) trees
			   else [(1,1)]
			   handle _ => [(1,1)],
	  targetScope   = getTargetScopeMaybe atts,
	  ownerScope    = getOwnerScopeMaybe atts,
	  taggedValue   = (map mkTaggedValue 
				 (XmlTree.follow "UML:ModelElement.taggedValue" trees)) }
    in XmlTree.apply_on "UML:Attribute" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkAttribute: "^msg)
	    | _ => raise IllFormed ("Error in mkAttribute")

    end

fun mkTagDefinition tree =
    let fun f atts trees = { xmiid = getXmiId atts,
			     name = getName atts,
			     multiplicity = (mkMultiplicity o hd o 
					     (XmlTree.follow "UML:TagDefinition.multiplicity")) trees }
    in XmlTree.apply_on "UML:TagDefinition" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkTagDefinition: "^msg)
    end

fun mkStereotypeR tree = 
    let fun f atts trees =   getXmiIdref atts
    in XmlTree.apply_on "UML:Stereotype" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkStereotype: "^msg)
    end 

fun mkProcedure tree = 
    let fun f atts trees = XMI.mk_Procedure 
			       {xmiid = getXmiId atts,
				name = getMaybeEmptyName atts,
				isSpecification = getBoolAtt "isSpecification" atts,
				isAsynchronous = false, (* FIX *)
				language = getLang atts,
				body = getBody atts,
				expression = nil (*FIX *)}
    in XmlTree.apply_on "UML:Procedure" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkGuard: "^msg)
    end
	
(* UML1.4 specific?
fun mkProcedure tree =
    let fun get_AttrL x = (XmlTree.attributes_of o (XmlTree.find "UML:ActionExpression") o
                        XmlTree.node_children_of o (XmlTree.find "UML:Action.script")) x
        fun f atts trees = XMI.mk_Procedure{
                               xmiid = getXmiId atts,
                               name  = getMaybeEmptyName atts,
                               isSpecification = getBoolAtt "isSpecification" atts,
                               isAsynchronous  = getBoolAtt "isAsynchronous"  atts,
                               language        = getLang(get_AttrL trees),
                               body            = getBody(get_AttrL trees),
                               expression      = nil}
    in XmlTree.apply_on "UML:CallAction" f tree 
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkProcedure: "^msg)
    (* POSEIDON specific ! According to UML 1.5, should be: "UML:Method" *)
    end
*)

fun mkGuard tree = 
    let val getExpr =  XmlTree.node_children_of o (XmlTree.find "UML:Guard.expression")
		val getBoolExpr = XmlTree.attributes_of o (XmlTree.find "UML:BooleanExpression") o
                      XmlTree.node_children_of o (XmlTree.find "UML:Guard.expression") 
        val getOclExpr = (XmlTree.find "UML:ExpressionInOcl") o
                      XmlTree.node_children_of o (XmlTree.find "UML:Guard.expression")
        fun f atts trees = XMI.mk_Guard{
                               xmiid = getXmiId atts,
                               name  = getMaybeEmptyName atts,
                               isSpecification = getBoolAtt "isSpecification" atts,
                               visibility      = getVisibility atts,
                               language        = if XmlTree.exists "UML:ExpressionInOCL" (getExpr trees) 
												 then getLang(XmlTree.attributes_of (getOclExpr trees))
												 else getLang(getBoolExpr trees),
                               body            = if XmlTree.exists "UML:ExpressionInOCL" (getExpr trees) 
												 then NONE 
												 else SOME (getBody (getBoolExpr trees)) ,
                               expression      = if XmlTree.exists "UML:ExpressionInOCL" (getExpr trees) 
												 then SOME (mkOCLExpression (getOclExpr trees))
												 else NONE}
    in   XmlTree.apply_on "UML:Guard" f tree 
	 handle XmlTree.IllFormed msg => raise IllFormed ("in mkGuard: "^msg)
    end


fun mkTransition tree = 
    let val getGuard     = (Option.map (mkGuard  o 
					(XmlTree.find "UML:Guard") o
					XmlTree.node_children_of))  o
                           (XmlTree.find_some "UML:Transition.guard")
	val getTrigger     = (Option.map (getXmiIdref o XmlTree.attributes_of o
					 hd o XmlTree.node_children_of))  o
			    (XmlTree.find_some "UML:Transition.trigger")
        val getTagVal    = List.concat o 
                           (map ((map mkTaggedValue) o XmlTree.node_children_of)) o 
                           (XmlTree.filter "UML:ModelElement.taggedValue")
	val getEffect    = (Option.map (mkProcedure o 
					(XmlTree.find "UML:Procedure") o
					XmlTree.node_children_of)) o
			   (XmlTree.find_some "UML:Transition.effect")
        fun f atts trees = XMI.mk_Transition  
                           {isSpecification = getBoolAtt "isSpecification" atts,
                            xmiid  = getXmiId atts,
                            source = (getXmiIdref o XmlTree.attributes_of o 
                                      hd o XmlTree.node_children_of o 
                                      (XmlTree.find "UML:Transition.source"))
                                     (trees),
                            target = (getXmiIdref o XmlTree.attributes_of o 
                                      hd o XmlTree.node_children_of o 
                                      (XmlTree.find "UML:Transition.target"))
                                     (trees),
			    guard  = getGuard trees, 
			    trigger= getTrigger trees,
		            effect = getEffect trees,
                            taggedValue = getTagVal trees}
    in XmlTree.apply_on "UML:Transition" f tree 
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkTransition: "^msg)
    end



fun mkState tree =
    let val elem         = XmlTree.tagname_of    tree
	val atts         = XmlTree.attributes_of tree
	val trees        = XmlTree.node_children_of   tree
        val xmiid        = getXmiId atts
        val name         = Option.getOpt(XmlTree.attvalue_of "name" atts,"")
        val isSpecification =  getBoolAtt "isSpecification" atts
        val getTid       = getXmiIdref o XmlTree.attributes_of
        fun getTrans str = List.concat o 
                           (map ((map getTid) o XmlTree.node_children_of)) o
                           (XmlTree.filter str)
        val getIncoming  = getTrans "UML:StateVertex.incoming"
        val getOutgoing  = getTrans "UML:StateVertex.outgoing"
        val getSubvertex = (map mkState) o XmlTree.node_children_of o 
                           (XmlTree.find "UML:CompositeState.subvertex")
        val getEntry     = (Option.map (mkProcedure  o 
					(XmlTree.find "UML:CallAction") o
					XmlTree.node_children_of))  o
                           (XmlTree.find_some "UML:State.entry")
        val getExit     = (Option.map (mkProcedure  o 
					(XmlTree.find "UML:CallAction") o
					XmlTree.node_children_of))  o
                           (XmlTree.find_some "UML:State.exit")
        val getDo     = (Option.map (mkProcedure  o 
					(XmlTree.find "UML:CallAction") o
					XmlTree.node_children_of))  o
                           (XmlTree.find_some "UML:State.doActivity")
        val getTagVal    = List.concat o 
                           (map ((map mkTaggedValue) o XmlTree.node_children_of)) o 
                           (XmlTree.filter "UML:ModelElement.taggedValue")
        val getStereo    = List.concat o 
                           (map ((map mkStereotypeR) o XmlTree.node_children_of)) o 
                           (XmlTree.filter "UML:ModelElement.stereotype")
	fun getType t      = map (getTid o hd o XmlTree.node_children_of) (XmlTree.filter "UML:ObjectFlowState.type" (XmlTree.node_children_of t))

(*
        val visibility = getVisibility atts
 *)
    in  case elem of 
        "UML:CompositeState" => 
             XMI.CompositeState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    isConcurrent = getBoolAtt "isConcurrent" atts,
                    outgoing     = getOutgoing trees, incoming = getIncoming trees, 
	            subvertex    = getSubvertex trees,
                    entry        = getEntry trees,
                    exit         = getExit trees,
                    doActivity   = getDo trees,
                    taggedValue  = getTagVal trees}
        |"UML:SubactivityState" => 
             XMI.SubactivityState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    isConcurrent = getBoolAtt "isConcurrent" atts,
                    isDynamic    = getBoolAtt "isDynamic" atts,
                    outgoing     = getOutgoing trees, incoming = getIncoming trees, 
	            subvertex    = getSubvertex trees,
                    entry        = getEntry trees,
                    exit         = getExit trees,
                    doActivity   = getDo trees,
                    submachine   = mkStateMachine (hd trees) 
                    (* HACK ! So far, no UML tool supports this. Parser has to be adapted
                       of we find a first example ... *),
                    taggedValue  = getTagVal trees}
       |"UML:ActionState" => 
             XMI.ActionState {
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    outgoing     = getOutgoing trees, incoming = getIncoming trees, 
                    isDynamic    = getBoolAtt "isDynamic" atts,
                    entry        = getEntry trees,
                    taggedValue  = getTagVal trees}
       |"UML:Pseudostate" => 
             XMI.PseudoState {
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    kind         = getPseudoStateKindAttr atts,
                    outgoing     = getOutgoing trees,incoming = getIncoming trees,
                    taggedValue  = getTagVal trees}
       |"UML:SimpleState" => 
             XMI.SimpleState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    entry        = getEntry trees,
                    exit         = getExit trees,
                    doActivity   = getDo trees,
                    outgoing     = getOutgoing trees, incoming = getIncoming trees,
                    taggedValue  = getTagVal trees}
       |"UML:ObjectFlowState" => 
             XMI.ObjectFlowState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    entry        = getEntry trees,
                    exit         = getExit trees,
                    doActivity   = getDo trees,
                    outgoing     = getOutgoing trees, incoming = getIncoming trees, 
                    isSynch      = getBoolAtt "isSynch" atts,
                    parameter    = nil,
                    type_        = hd (getType tree),
                    taggedValue  = getTagVal trees}
       |"UML:FinalState" => 
             XMI.FinalState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    entry        = getEntry trees,
                    exit         = getExit trees,
                    doActivity   = getDo trees,
                    incoming = getIncoming trees,
                    taggedValue  = getTagVal trees}
       |"UML:SyncState" => 
             XMI.SyncState{
                    xmiid=xmiid,name=name,isSpecification=isSpecification,
                    stereotype   = getStereo trees,
                    bound        = 0,
                    outgoing     = getOutgoing trees,incoming = getIncoming trees,
                    taggedValue  = getTagVal trees}

       | _ => raise IllFormed ("in mkState: Unknown State Vertex.")
     end
	handle _ => raise IllFormed ("Error in mkState")
					     

and mkStateMachine tree =
    let fun f atts trees = XMI.mk_StateMachine 
                           {isSpecification = getBoolAtt "isSpecification" atts,
                            xmiid        = getXmiId atts, 
                            contextxmiid = (getXmiIdref o XmlTree.attributes_of o hd o 
                                            XmlTree.node_children_of o 
                                            (XmlTree.find "UML:StateMachine.context"))
                                           (trees), 
                            top          = (hd o (map mkState) o XmlTree.node_children_of o 
                                           (XmlTree.find "UML:StateMachine.top"))
                                           (trees),
                            transitions  = ((map mkTransition) o XmlTree.node_children_of o 
                                           (XmlTree.find "UML:StateMachine.transitions"))
                                           (trees)}
    in  XmlTree.apply_on "UML:StateMachine" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkStateMachine: "^msg)
	    | _ => raise IllFormed ("Error in mkStateMachine")
    end;





fun mkActivityGraph tree =
    let fun f atts trees = XMI.mk_ActivityGraph 
                           {isSpecification = getBoolAtt "isSpecification" atts,
                            xmiid        = getXmiId atts,
                            contextxmiid = (getXmiIdref o XmlTree.attributes_of o hd o 
                                            XmlTree.node_children_of o 
                                            (XmlTree.find "UML:StateMachine.context"))
                                           (trees), 
                            top          = (hd o (map mkState) o XmlTree.node_children_of o 
                                           (XmlTree.find "UML:StateMachine.top"))
                                           (trees),
                            transitions  = ((map mkTransition) o XmlTree.node_children_of o 
                                           (XmlTree.find "UML:StateMachine.transitions"))
                                           (trees),
                            partition    = nil}

    in  XmlTree.apply_on "UML:ActivityGraph" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkActivityGraph: "^msg)
    end;
	    
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
	    attributes      = (map mkAttribute 
				   ((XmlTree.filter "UML:Attribute") 
					(XmlTree.follow "UML:Classifier.feature"
							trees))),
	    operations      = (map mkOperation 
				   ((XmlTree.filter "UML:Operation") 
					(XmlTree.follow "UML:Classifier.feature"
							trees))),
	    invariant       = (map (getXmiIdref o XmlTree.attributes_of)
				   (XmlTree.follow "UML:ModelElement.constraint" 
						   trees)),
	    stereotype      = (map (getXmiIdref o XmlTree.attributes_of) 
				   (XmlTree.follow "UML:ModelElement.stereotype" 
						   trees)),
	    taggedValue   = (map mkTaggedValue 
				 (XmlTree.follow "UML:ModelElement.taggedValue" 
						 trees)),
	    clientDependency = (map (getXmiIdref o XmlTree.attributes_of)
				    (XmlTree.follow "UML:ModelElement.clientDependency"
						    trees)),
	    supplierDependency = (map (getXmiIdref o XmlTree.attributes_of)
				      (XmlTree.follow "UML:ModelElement.supplierDependency"
						      trees)),
	    classifierInState = (map (getXmiId o XmlTree.attributes_of)
				     (XmlTree.filter "UML:ClassifierInState"
						     (XmlTree.follow "UML:Namespace.ownedElement"
								     trees))),
	    state_machines = (map mkStateMachine (XmlTree.filter "UML:StateMachine"
				   (XmlTree.follow "UML:Namespace.ownedElement" trees))),
	    activity_graphs = (map mkActivityGraph (XmlTree.filter "UML:ActivityGraph"
				   (XmlTree.follow "UML:Namespace.ownedElement" trees))) }
    handle IllFormed msg => raise IllFormed ("Error in mkClass "^((getName atts)
								  ^": "^msg))
	 | _ => raise IllFormed ("Error in mkClass "^(getName atts))

fun mkAssociationClass atts trees 
  = XMI.AssociationClass { xmiid           = getXmiId atts,
			   name            = getName atts,
			   isActive        = getBoolAtt "isActive" atts,
			   visibility      = getVisibility atts,
			   isLeaf          = getBoolAtt "isLeaf" atts,
	    generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
				   (XmlTree.follow_all 
					"UML:GeneralizableElement.generalization" 
					trees)),
	    attributes      = (map mkAttribute 
				   ((XmlTree.filter "UML:Attribute") 
					(XmlTree.follow "UML:Classifier.feature"
							trees))),
	    operations      = (map mkOperation 
				   ((XmlTree.filter "UML:Operation") 
					(XmlTree.follow "UML:Classifier.feature"
							trees))),
	    invariant       = (map (getXmiIdref o XmlTree.attributes_of)
				   (XmlTree.follow "UML:ModelElement.constraint" 
						   trees)),
	    stereotype      = (map (getXmiIdref o XmlTree.attributes_of) 
				   (XmlTree.follow "UML:ModelElement.stereotype" 
						   trees)),
	    taggedValue   = (map mkTaggedValue 
				 (XmlTree.follow "UML:ModelElement.taggedValue" 
						 trees)),
	    clientDependency = (map (getXmiIdref o XmlTree.attributes_of)
				    (XmlTree.follow "UML:ModelElement.clientDependency"
						    trees)),
	    supplierDependency = (map (getXmiIdref o XmlTree.attributes_of)
				      (XmlTree.follow "UML:ModelElement.supplierDependency"
						      trees)),
	    connection = (map mkAssociationEnd (XmlTree.follow "UML:Association.connection" 
							       trees))}
    handle _ => raise IllFormed ("Error in mkClass")


fun mkPrimitive atts trees 
  = XMI.Primitive { xmiid      = getXmiId atts,
		    name       = getName atts,
		    operations = (map mkOperation 
				      ((XmlTree.filter "UML:Operation") 
					   (XmlTree.follow "UML:Classifier.feature"
							   trees))),
		    generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
					   (XmlTree.follow_all 
						"UML:GeneralizableElement.generalization" 
						trees)),
		    invariant = (map (getXmiIdref o XmlTree.attributes_of)
				     (XmlTree.follow "UML:ModelElement.constraint" 
						     trees))
				     }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkPrimitive: "^msg)

fun mkInterface atts trees 
  = XMI.Interface { xmiid      = getXmiId atts,
		    name       = getName atts,
		    operations = (map mkOperation 
				      ((XmlTree.filter "UML:Operation") 
					   (XmlTree.follow "UML:Classifier.feature"
							   trees))),
		    generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
					   (XmlTree.follow_all 
						"UML:GeneralizableElement.generalization" 
						trees)),
		    invariant = (map (getXmiIdref o XmlTree.attributes_of)
				     (XmlTree.follow "UML:ModelElement.constraint" 
						     trees)),
		    clientDependency = (map (getXmiIdref o XmlTree.attributes_of)
					    (XmlTree.follow "UML:ModelElement.clientDependency"
							    trees)),
		    supplierDependency = (map (getXmiIdref o XmlTree.attributes_of)
					      (XmlTree.follow "UML:ModelElement.supplierDependency"
							      trees))
		    }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkPrimitive: "^msg)
					  
fun mkEnumeration atts trees 
  = XMI.Enumeration { xmiid      = getXmiId atts,
		      name       = getName atts,
		      operations = (map mkOperation 
					((XmlTree.filter "UML:Operation") 
					     (XmlTree.follow "UML:Classifier.feature"
							     trees))),
		      generalizations = (map (getXmiIdref o XmlTree.attributes_of o hd)
					     (XmlTree.follow_all 
						  "UML:GeneralizableElement.generalization" 
						  trees)),
		      literals = nil, (* FIX *)
		      invariant = (map (getXmiIdref o XmlTree.attributes_of)
				       (XmlTree.follow "UML:ModelElement.constraint" 
						       trees)
				       )}
		      handle XmlTree.IllFormed msg => raise IllFormed ("in mkEnumeration: "^msg)
							    
fun mkVoid atts trees = XMI.Void { xmiid = getXmiId atts, 
				       name  = getName atts }
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkVoid: "^msg)


fun mkGenericCollection atts trees = 
    { xmiid      = getXmiId atts,
      name       = getName atts,
      operations = (map mkOperation 
			((XmlTree.filter "UML:Operation") 
			     (XmlTree.follow "UML:Classifier.feature"
					     trees))),
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

fun mkStereotype tree = 
    let fun f atts trees =  { xmiid = getXmiId atts,
			      name = getName atts,
			      baseClass = NONE, (*FIX*)
			      stereotypeConstraint = NONE (*FIX*)
			  }
    in XmlTree.apply_on "UML:Stereotype" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkStereotype: "^msg)
    end 


fun mkClassifier tree = 
    let val elem  = XmlTree.tagname_of    tree
	val atts  = XmlTree.attributes_of tree
	val trees = XmlTree.node_children_of    tree
    in 
	case elem of "UML:Class"                     => mkClass atts trees
		   | "UML:AssociationClass"          => mkAssociationClass atts trees
		   | "UML:Interface"                 => mkInterface atts trees
		   | "UML:DataType"                  => mkPrimitive atts trees
		   | "UML:Primitive"                 => mkPrimitive atts trees
		   | "UML:Enumeration"               => mkEnumeration atts trees
		   | "UML15OCL.Types.VoidType"       => mkVoid atts trees
		   | "UML15OCL.Types.CollectionType" => mkCollection atts trees
		   | "UML15OCL.Types.SequenceType"   => mkSequence atts trees
		   | "UML15OCL.Types.SetType"        => mkSet atts trees
		   | "UML15OCL.Types.BagType"        => mkBag atts trees
		   | "UML15OCL.Types.OrderedSetType" => mkOrderedSet atts trees
		   | _ => raise IllFormed ("in mkClassifier: found unexpected element "^elem)
    end


    
fun mkGeneralization tree = 
    let fun f atts trees =
	{ xmiid     = getXmiId atts,
	  child_id  = (getXmiIdref o XmlTree.attributes_of o hd o
		       (XmlTree.follow "UML:Generalization.child")) trees, 
	  parent_id = (getXmiIdref o XmlTree.attributes_of o hd o 
		       (XmlTree.follow "UML:Generalization.parent")) trees }
	handle Empty => raise IllFormed ("Empty in mkGeneralization: "^getXmiId atts)
    in XmlTree.apply_on "UML:Generalization" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkGeneralization: "^msg)
	    
    end


fun mkCallEvent atts trees =
    XMI.CallEvent {xmiid = getXmiId atts,
		   name = getMaybeEmptyName atts,
		   operation = (getXmiIdref o XmlTree.attributes_of o hd o 
		       (XmlTree.follow "UML:CallEvent.operation")) trees,
		   parameter = (map mkParameter 
			       (XmlTree.follow "UML:Event.parameter" 
					       trees))
		   }

(* TODO: mkSignalEvent, etc ? *)
fun mkEvent tree = 
    let val elem  = XmlTree.tagname_of    tree
	val atts  = XmlTree.attributes_of tree
	val trees = XmlTree.node_children_of    tree
    in 
	case elem of "UML:CallEvent"                     => mkCallEvent atts trees
		   | _ => raise IllFormed ("in mkEvent: found unexpected element "^elem)
    end
	handle _ => raise IllFormed ("Error in mkEvent")




fun mkPackage tree = 
    (if XmlTree.tagname_of tree = "UML:Model" orelse
	XmlTree.tagname_of tree = "UML:Package" then 
	 let val direct_childs = XmlTree.node_children_of tree
	     val trees = XmlTree.follow "UML:Namespace.ownedElement" 
				      (XmlTree.node_children_of tree)
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
			       constraints     = (map mkConstraint 
						     (filterConstraints trees)),
			       stereotypes     = (map mkStereotype
						      (filterStereotypes trees)),
			       tag_definitions = (map mkTagDefinition
						      (filterTagDefinitions trees)),
                               state_machines  = nil,
                               activity_graphs = nil,
			       dependencies    = (map mkDependency (filterDependencies trees)),
			       stereotype = (map (getXmiIdref o XmlTree.attributes_of) 
				   (XmlTree.follow "UML:ModelElement.stereotype" 
						   direct_childs)), 
			       taggedValue = (map mkTaggedValue 
						  (XmlTree.follow "UML:ModelElement.taggedValue" 
								  direct_childs)),
			       events =  map mkEvent (filterEvents trees)
                              }
(*		 handle Empty => raise IllFormed ("Error Empty in mkPackage "^(getName atts))*)
	 end
     else raise IllFormed "did not find a UML:Model or UML: Package")
    handle XmlTree.IllFormed msg => raise IllFormed ("in mkPackage: "^msg)
				 

fun mkXmiContent tree =
    let fun f atts trees = 
	    { packages    = (map mkPackage    (filterPackages trees)),
	      constraints = (map mkConstraint (filterConstraints trees)),
	      classifiers = (map mkClassifier (filterClassifiers trees)),
	      stereotypes = (map mkStereotype (filterStereotypes trees)),
	      variable_declarations = map mkVariableDec (filterVariableDecs trees),
              activity_graphs = map mkActivityGraph(filterActivityGraphs trees),
              state_machines  = map mkStateMachine (filterStateMachines trees)}
    in XmlTree.apply_on "XMI.content" f tree
       handle XmlTree.IllFormed msg => raise IllFormed ("in mkXmiContent: "^msg)
    end
	



val emptyXmiContent = { packages              = nil,
			constraints           = nil,
			classifiers           = nil,
			stereotypes           = nil,
			variable_declarations = nil,
                        activity_graphs       = nil,
                        state_machines        = nil}

fun findXmiContent tree = valOf (XmlTree.dfs "XMI.content" tree)
    handle Option => raise IllFormed "in findXmiContent: did not find XMI.content"
			       
fun readFile f = (mkXmiContent o findXmiContent o ParseXmlTree.readFile) f
    handle XmlTree.IllFormed msg =>  (print ("Warning: "^msg^"\n"); emptyXmiContent)
	 | IllFormed msg => (print ("Warning: "^msg^"\n"); emptyXmiContent)
end


