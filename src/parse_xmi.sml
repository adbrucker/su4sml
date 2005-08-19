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

structure ParseXMI =
struct

(* generic exception if something is wrong *)
exception IllFormed of string 

exception NotYetImplemented


fun getAttValueMaybe string atts = Option.map #2 (find (fn (x,_) => x = string) 
						       atts)

fun getAttValue string atts = valOf (getAttValueMaybe string atts)
    handle Option => raise IllFormed ("in getAttValue: did not find attribute "^string)

fun getBoolAtt string atts = 
    let val att = getAttValue string atts
    in 
	(valOf o Bool.fromString) att 
	handle Option => raise IllFormed ("in getBoolAtt: found attribute "^string^
					  " with unexpected value "^att)
    end


fun getIntAtt string atts = 	  
    let val att = getAttValue string atts
    in 
	(valOf o Int.fromString) att 
	handle Option => raise IllFormed ("in getIntAtt: found attribute "^string^
					  " with unexpected value "^att)
    end

fun getXmiId    a = getAttValue "xmi.id"    a
fun getName     a = getAttValue "name"      a
fun getXmiIdref a = getAttValue "xmi.idref" a
		 
fun getVisibility atts = 
    let val att = getAttValueMaybe "visibility" atts 
    in
	case att of SOME "public"    => XMI_UML.public
		  | SOME "private"   => XMI_UML.private
		  | SOME "protected" => XMI_UML.protected
		  | SOME "package"   => XMI_UML.package
		  | NONE             => XMI_UML.public
		  | SOME string      => raise IllFormed ("in getVisibility: found unexpected attribute value "^string)
    end

fun getOrdering atts = 
    let val att = getAttValue "ordering" atts 
    in 
	case att of "unordered" => XMI_UML.Unordered
		  | "ordered"  => XMI_UML.Ordered
		  | _ => raise IllFormed ("in getOrdering: found unexpected attribute value "^att)
    end 

fun getAggregation atts = 
    let val att = getAttValue "aggregation" atts in
	case att of "none" => XMI_UML.NoAggregation
		  | "aggregate" => XMI_UML.Aggregate
		  | "composite" => XMI_UML.Composite
		  | _ => raise IllFormed ("in getAggregation: found unexpected attribute value "^att)
    end 

fun getChangeability atts = 
    let val att = getAttValue "changeability" atts in
	case att of "changeable" => XMI_UML.Changeable
		  | "frozen"     => XMI_UML.Frozen
		  | "addonly"    => XMI_UML.AddOnly
		  | _ => raise IllFormed ("in getChangeability: found unexpected attribute value "^att)
    end 
			       
fun getKind atts = 
    let val att = getAttValue "kind" atts in
	case att of "in"     => XMI_UML.In
		  | "out"    => XMI_UML.Out
		  | "inout"  => XMI_UML.Inout
		  | "return" => XMI_UML.Return
		  | _ => raise IllFormed ("in getKind: found unexpected attribute value "^att)
    end 
	
fun getRange atts = (getIntAtt "lower" atts, getIntAtt "upper" atts)


fun skipOver string tree = if string = XmlTreeData.getElem tree 
			   then XmlTreeData.getTrees tree
			   else raise IllFormed ("in skipOver: did not find element "^string)
      
fun filterByName string trees = List.filter (fn x => string = XmlTreeData.getElem x) 
				       trees

fun findByName string trees = valOf (find (fn x => string = XmlTreeData.getElem x)
					  trees) 
    handle Option => raise IllFormed ("in findByName: did not find element "^string)

fun existsByName string trees = List.exists (fn x => string = XmlTreeData.getElem x) 
					    trees 
					    
fun followByName string = (skipOver string) o (findByName string)
			  
fun followAllByName string trees = map (skipOver string) 
				       (filterByName string trees) 


fun generic_tree2xmi name constructor tree = 
    (if XmlTreeData.getElem tree = name
     then constructor (XmlTreeData.getAtts tree)
		      (XmlTreeData.getTrees tree)
     else raise IllFormed ("in generic_tree2xmi: did not find element "^name))

fun mkAssociationEnd atts trees =
    { xmiid = getXmiId atts, name = getName atts, 
      isNavigable  = getBoolAtt "isNavigable" atts,
      ordering     = getOrdering atts,
      aggregation  = getAggregation atts,
      multiplicity = map (getRange o XmlTreeData.getAtts) 
			 (((filterByName "UML:MultiplicityRange") o 
			   (skipOver "UML:Multiplicity.range") o hd o 
			   (skipOver "UML:Multiplicity") o hd o 
			   (followByName "UML:AssociationEnd.multiplicity"))
			      trees),
      changeability = getChangeability atts, 
      visibility    = getVisibility atts,
      participant_id = (getXmiIdref o XmlTreeData.getAtts o hd o
			(followByName "UML:AssociationEnd.participant")) trees }
    handle IllFormed msg => raise IllFormed ("in mkAssociationEnd: "^msg)

fun tree2aend tree           = generic_tree2xmi "UML:AssociationEnd" 
						mkAssociationEnd tree

fun mkAssociation atts trees = 
    { xmiid = getXmiId atts, name = getName atts,
      connection = map tree2aend (skipOver "UML:Association.connection" 
					   (hd trees)) }
    handle IllFormed msg => raise IllFormed ("in mkAssociation: "^msg)

fun tree2association tree    = generic_tree2xmi "UML:Association" 
						mkAssociation tree



(* find the xmi.idref attribute of an element pinted to by name *)
fun findXmiIdRef name trees = (getXmiIdref o XmlTreeData.getAtts o hd)
				  (followByName name trees)

(* find the type of an OCl sub-expression *)
fun findExpressionType trees = findXmiIdRef "OCL.Expressions.OclExpression.type" 
					    trees
			       

fun tree2oclexpression tree = 
    let val elem  = XmlTreeData.getElem tree
	val atts  = XmlTreeData.getAtts tree
	val trees = XmlTreeData.getTrees tree
    in 
	if elem = "UML15OCL.Expressions.BooleanLiteralExp" then
	    XMI_UML.LiteralExp { symbol          = getAttValue "booleanSymbol" atts,
			 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.IntegerLiteralExp" then
	    XMI_UML.LiteralExp { symbol          = getAttValue "integerSymbol" atts,
			 expression_type = findExpressionType trees }
	else if elem = "UML15OCL.Expressions.OperationCallExp" then
	    let val op_src = hd (followByName 
				      "OCL.Expressions.PropertyCallExp.source"
				      trees)
		val op_ref = 
		    findXmiIdRef 
			"OCL.Expressions.OperationCallExp.referredOperation" trees
		val op_args = followAllByName 
				  "OCL.Expressions.OperationCallExp.arguments"
				  trees 
	    in XMI_UML.OperationCallExp 
		   { source            = tree2oclexpression op_src,
		     arguments         = map (tree2oclexpression o hd) op_args,
		     referredOperation = op_ref,
		     expression_type   = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.AttributeCallExp" then
	    let val att_ref = 
		    findXmiIdRef 
			"OCL.Expressions.AttributeCallExp.referredAttribute" trees
		val att_src = (hd o followByName 
					"OCL.Expressions.PropertyCallExp.source") 
				  trees
	    in XMI_UML.AttributeCallExp 
		   { source            = tree2oclexpression att_src,
		     referredAttribute = att_ref,
		     expression_type   = findExpressionType trees }
    end
	else if elem = "UML15OCL.Expressions.AssociationEndCallExp" then
	    let val assoc_src = (hd o followByName 
					  "OCL.Expressions.PropertyCallExp.source") 
				    trees
		val assoc_ref = 
		    findXmiIdRef 
			"OCL.Expressions.AssociationEndCallExp.referredAssociationEnd"
			trees
	    in XMI_UML.AssociationEndCallExp 
		   { source = tree2oclexpression assoc_src,
		     referredAssociationEnd  = assoc_ref,
		     expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.AssociationEndCallExp" then
	    raise NotYetImplemented
	else if elem = "UML15OCL.Expressions.VariableExp" then
	    let val var_ref = findXmiIdRef
				  "OCL.Expressions.VariableExp.referredVariable"
				  trees
	    in XMI_UML.VariableExp { referredVariable = var_ref,
			     expression_type  = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.IfExp" then
	    let val cond = (hd o followByName 
				     "OCL.Expressions.IfExp.condition") trees
		val then_exp = (hd o followByName
					 "OCL.Expressions.IfExp.thenExpression") 
				   trees
		val else_exp = (hd o followByName 
					 "OCL.Expressions.IfExp.elseExpression")
				   trees
	    in XMI_UML.IfExp { condition      = tree2oclexpression cond,
		       thenExpression = tree2oclexpression then_exp,
		       elseExpression = tree2oclexpression else_exp,
		       expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.LetExp" then 
	    let val var_decl = (hd o followByName 
					 "OCL.Expressions.LetExp.variable") trees
		val var_xmiid    = getXmiId (XmlTreeData.getAtts var_decl)
		val var_name     = getName (XmlTreeData.getAtts var_decl)
		val var_type_ref = findXmiIdRef 
				       "OCL.Expressions.VariableDeclaration.type" 
				       (XmlTreeData.getTrees var_decl)
		val in_exp = (hd o followByName "OCL.Expressions.LetExp.in") trees
		val init_exp = 
		    (hd o followByName 
			      "OCL.Expressions.VariableDeclaration.initExpression") 
			(XmlTreeData.getTrees var_decl)
	    in XMI_UML.LetExp 
		   { variable        = { xmiid            = var_xmiid,
					 name             = var_name, 
					 declaration_type = var_type_ref },
		     initExpression  = tree2oclexpression init_exp ,
		     inExpression    = tree2oclexpression in_exp,
		     expression_type = findExpressionType trees }
	    end
	else if elem = "UML15OCL.Expressions.IterateExp"  then 
	    raise NotYetImplemented
	else if elem = "UML15OCL.Expressions.IteratorExp" then 
	    raise NotYetImplemented
	else raise IllFormed ("in tree2oclexpression: found unexpected element "^elem)
    end

fun getAssociations t = map tree2association ((filterByName "UML:Association") t)
			
val filterConstraints  = filterByName "UML:Constraint"


fun mkConstraint atts trees = 
    let val expr = (hd o (followByName 
			      "OCL.Expressions.ExpressionInOcl.bodyExpression") o
		    (followByName "UML15OCL.Expressions.ExpressionInOcl") o 
		    (followByName "UML:Constraint.body"))
		       trees
	val st_type = hd (followByName "UML:ModelElement.stereotype" trees)
	val st_type_ref  =  getXmiIdref (XmlTreeData.getAtts st_type)
    in { xmiid = getXmiId atts,
	 name  = case getAttValueMaybe "name" atts of SOME s => SOME s | _ => NONE,
	 constraint_type = st_type_ref, 
	 body = tree2oclexpression expr }
    end
    handle IllFormed msg => raise IllFormed ("in mkConstraint: "^msg)

fun tree2constraint tree = generic_tree2xmi "UML:Constraint" mkConstraint tree

    
fun filterPackages t = append (filterByName "UML:Package" t)
			      (filterByName "UML:Model" t)
			      
(* FIX: other classifiers *) 
fun filterClassifiers trees = 
    filter (fn x => let val elem = XmlTreeData.getElem x in
			elem = "UML:Class"                     orelse
			elem = "UML:Primitive"                 orelse
			elem = "UML:DataType"                  orelse
			elem = "UML:Interface"                 orelse
			elem = "UML:Enumeration"               orelse
			elem = "UML15OCL.Types.SequenceType"   orelse
			elem = "UML15OCL.Types.BagType"        orelse
			elem = "UML15OCL.Types.SetType"        orelse
			elem = "UML15OCL.Types.CollectionType" orelse
			elem = "UML15OCL.Types.VoidType" 
		    end) trees

fun mkParameter atts trees = { xmiid   = getXmiId atts,
			       name    = getName atts,
			       kind    = getKind atts,
			       type_id = (getXmiIdref o 
					  XmlTreeData.getAtts o hd o 
					  (followByName "UML:Parameter.type")) 
					     trees }
    handle IllFormed msg => raise IllFormed ("in mkParameter: "^msg)

fun tree2parameter tree      = generic_tree2xmi "UML:Parameter" mkParameter tree

fun mkOperation atts trees =
    { xmiid         = getXmiId atts,
      name          = getName atts,
      visibility    = getVisibility atts,
      isQuery       = getBoolAtt "isQuery" atts,
      parameter     = (map tree2parameter 
			   (followByName "UML:BehavioralFeature.parameter" 
					 trees)),
      constraints   = if existsByName "UML:ModelElement.constraint" trees
		      then map (getXmiIdref o XmlTreeData.getAtts)
			       (followByName "UML:ModelElement.constraint" 
					     trees)
		      else nil}
    handle IllFormed msg => raise IllFormed ("in mkOperation: "^msg)

fun tree2operation tree      = generic_tree2xmi "UML:Operation" mkOperation tree

fun mkAttribute atts trees =
    { xmiid         = getXmiId atts,
      name          = getName atts,
      visibility    = getVisibility atts,
      changeability = getChangeability atts,
      type_id       = (getXmiIdref o XmlTreeData.getAtts o hd o 
		       (followByName "UML:StructuralFeature.type")) trees }
    handle IllFormed msg => raise IllFormed ("in mkAttribute: "^msg)

fun tree2attribute tree      = generic_tree2xmi "UML:Attribute" mkAttribute tree

fun mkClass atts trees 
  = XMI_UML.Class { xmiid           = getXmiId atts,
	    name            = getName atts,
	    isActive        = getBoolAtt "isActive" atts,
	    visibility      = getVisibility atts,
	    isLeaf          = getBoolAtt "isLeaf" atts,
	    generalizations = (map (getXmiIdref o XmlTreeData.getAtts o hd)
				  (followAllByName 
				       "UML:GeneralizableElement.generalization" 
				       trees)),
	    attributes      = if existsByName "UML:Classifier.feature" trees
			      then map tree2attribute 
				       ((filterByName "UML:Attribute") 
					    (followByName "UML:Classifier.feature"
							  trees))
			      else nil,
	    operations      = if existsByName "UML:Classifier.feature" trees
			      then map tree2operation 
				       ((filterByName "UML:Operation") 
					    (followByName "UML:Classifier.feature"
							  trees))
			      else nil,
	    invariant       = if existsByName "UML:ModelElement.constraint" trees
			      then map (getXmiIdref o XmlTreeData.getAtts)
				       (followByName "UML:ModelElement.constraint" 
						     trees)
			      else nil}

fun mkPrimitive atts trees 
  = XMI_UML.Primitive { xmiid      = getXmiId atts,
		name       = getName atts,
		operations = if existsByName "UML:Classifier.feature" trees
			     then map tree2operation 
				      ((filterByName "UML:Operation") 
					   (followByName "UML:Classifier.feature"
							 trees))
			     else nil,
		generalizations = (map (getXmiIdref o XmlTreeData.getAtts o hd)
				       (followAllByName 
					    "UML:GeneralizableElement.generalization" 
					    trees)),
		invariant = if existsByName "UML:ModelElement.constraint" trees
			    then map (getXmiIdref o XmlTreeData.getAtts)
				     (followByName "UML:ModelElement.constraint" 
						   trees)
			    else nil 
		}
    handle IllFormed msg => raise IllFormed ("in mkPrimitive: "^msg)
    
fun mkEnumeration atts trees 
  = XMI_UML.Enumeration { xmiid      = getXmiId atts,
		  name       = getName atts,
		  operations = if existsByName "UML:Classifier.feature" trees
			       then map tree2operation 
					((filterByName "UML:Operation") 
					     (followByName "UML:Classifier.feature"
							   trees))
			       else nil,
		  generalizations = (map (getXmiIdref o XmlTreeData.getAtts o hd)
					 (followAllByName 
					      "UML:GeneralizableElement.generalization" 
					      trees)),
		  literals = nil, (* FIX *)
		  invariant = if existsByName "UML:ModelElement.constraint" trees
			      then map (getXmiIdref o XmlTreeData.getAtts)
				       (followByName "UML:ModelElement.constraint" 
						     trees)
			      else nil
		  }
    handle IllFormed msg => raise IllFormed ("in mkEnumeration: "^msg)

fun mkVoid atts trees = XMI_UML.Void { xmiid = getXmiId atts, 
					  name = getName atts }
    handle IllFormed msg => raise IllFormed ("in mkVoid: "^msg)


fun mkGenericCollection atts trees = 
    { xmiid      = getXmiId atts,
      name       = getName atts,
      operations = if existsByName "UML:Classifier.feature" trees
		   then map tree2operation 
			    ((filterByName "UML:Operation") 
				 (followByName "UML:Classifier.feature"
					       trees))
		   else nil,
      generalizations = (map (getXmiIdref o XmlTreeData.getAtts o hd)
			     (followAllByName 
				  "UML:GeneralizableElement.generalization" 
				  trees)),
      elementtype = ((getXmiIdref o XmlTreeData.getAtts o hd) 
			 (followByName 
			      "OCL.Types.CollectionType.elementType" 
			      trees))
      }
    handle IllFormed msg => raise IllFormed ("in mkGenericCollection: "^msg)

    
fun mkCollection atts trees = XMI_UML.Collection (mkGenericCollection atts trees)
fun mkSequence   atts trees = XMI_UML.Sequence   (mkGenericCollection atts trees)
fun mkSet        atts trees = XMI_UML.Set        (mkGenericCollection atts trees)
fun mkBag        atts trees = XMI_UML.Bag        (mkGenericCollection atts trees)
fun mkOrderedSet atts trees = XMI_UML.OrderedSet (mkGenericCollection atts trees)

fun tree2classifier tree = 
    let val elem  = XmlTreeData.getElem tree
	val atts  = XmlTreeData.getAtts tree
	val trees = XmlTreeData.getTrees tree
    in 
	if elem = "UML:Class" then                          mkClass atts trees
	else if elem = "UML:Primitive" orelse 
		elem = "UML:DataType" then              mkPrimitive atts trees
	else if elem = "UML:Enumeration" then         mkEnumeration atts trees
	else if elem = "UML15OCL.Types.VoidType" then        mkVoid atts trees
	else if elem = "UML15OCL.Types.CollectionType" then 
	    mkCollection atts trees
	else if elem = "UML15OCL.Types.SequenceType" then mkSequence atts trees
	else if elem = "UML15OCL.Types.SetType" then           mkSet atts trees
	else if elem = "UML15OCL.Types.BagType" then           mkBag atts trees
	else if elem = "UML15OCL.Types.OrderedSetType" then
	    mkOrderedSet atts trees
	else raise IllFormed ("in tree2classifier: found unexpected element "^elem)
    end


    
fun mkGeneralization atts trees =
    { xmiid     = getXmiId atts,
      child_id  = (getXmiIdref o XmlTreeData.getAtts o hd o
		   (followByName "UML:Generalization.child")) trees, 
      parent_id = (getXmiIdref o XmlTreeData.getAtts o hd o 
		   (followByName "UML:Generalization.parent")) trees }
    handle IllFormed msg => raise IllFormed ("in mkGeneralization: "^msg)


fun tree2generalization tree = generic_tree2xmi "UML:Generalization" 
						mkGeneralization tree

    
fun tree2package tree = 
    (if XmlTreeData.getElem tree = "UML:Model" orelse
	XmlTreeData.getElem tree = "UML:Package" then 
	 let val trees = skipOver "UML:Namespace.ownedElement" 
				  ((hd o XmlTreeData.getTrees) tree)
	     val atts = XmlTreeData.getAtts tree in
	     XMI_UML.Package { xmiid   = getXmiId atts, 
				  name    = getName atts,
				  visibility      = getVisibility atts,
				  packages        = (map tree2package 
							 (filterPackages trees)), 
				  classifiers     = (map tree2classifier
							 (filterClassifiers trees)),
				  associations    = getAssociations trees, 
				  generalizations = (map tree2generalization
							 (filterByName "UML:Generalization"
								       trees)),
				 constraints     = map tree2constraint 
						       (filterConstraints trees) }
	 end
     else raise IllFormed "tree2package")
    handle IllFormed msg => raise IllFormed ("in mkPackage: "^msg)
					  

fun filterStereotypes trees = filterByName "UML:Stereotype" trees

fun mkStereotype atts trees = 
    { xmiid = getXmiId atts,
      name = getName atts,
      baseClass = NONE,
      stereotypeConstraint = NONE
     }
    handle IllFormed msg => raise IllFormed ("in mkStereotype: "^msg)

fun tree2stereotype tree = generic_tree2xmi "UML:Stereotype" mkStereotype tree

fun filterStereotypes trees = filterByName "UML:Stereotype" trees

fun filterVariableDecs trees = filterByName "UML15OCL.Expressions.VariableDeclaration" trees

fun mkVariableDec atts trees = 
    { xmiid = getXmiId atts,
      name = getName atts,
      declaration_type = (getXmiIdref o XmlTreeData.getAtts o hd o 
		   (followByName "OCL.Expressions.VariableDeclaration.type")) trees
     }
    handle IllFormed msg => raise IllFormed ("in mkVariableDec: "^msg)


fun tree2variable_dec tree = generic_tree2xmi "UML15OCL.Expressions.VariableDeclaration" mkVariableDec tree

fun mkXmiContent atts trees = 
    { packages = (map tree2package (filterPackages trees)),
      constraints = (map tree2constraint (filterConstraints trees)),
      classifiers = (map tree2classifier (filterClassifiers trees)),
      stereotypes = (map tree2stereotype (filterStereotypes trees)),
      variable_declarations = (map tree2variable_dec (filterVariableDecs trees))
      }
    handle IllFormed msg => raise IllFormed ("in mkXmiContent: "^msg)


fun tree2xmicontent tree = generic_tree2xmi "XMI.content" mkXmiContent tree


fun findXmiContent tree = if XmlTreeData.getElem tree = "XMI.content" 
			  then [tree]
			  else List.concat (map findXmiContent 
						(XmlTreeData.getTrees tree))
			       
fun parseXMI filename = 
    let val trees = findXmiContent (ParseXmlTree.readFile filename)
    in
	tree2xmicontent (hd trees)
    end
end


