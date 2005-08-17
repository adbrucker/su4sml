(*****************************************************************************
 *          su4sml - a shallow embedding of OCL in Isabelle/HOL              
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
(* open XMI_UML *)
exception IllFormed of string
exception NotYetImplemented

fun getAttValueMaybe string atts = Option.map #2 (find (fn (x,_) => x = string) 
						       atts)

fun getAttValue string atts = valOf (getAttValueMaybe string atts)
    handle Option => error ("error in getAttValue ("^string^")")

fun getBoolAtt string = valOf o Bool.fromString o 
			(getAttValue string)
    handle Option => error ("error in getBoolAtt ("^string^")")

	  
fun getXmiId    a = getAttValue "xmi.id"    a
fun getName     a = getAttValue "name"      a
fun getXmiIdref a = getAttValue "xmi.idref" a
		 
fun getVisibility atts = case getAttValueMaybe "visibility" atts 
			  of SOME "public"    => XMI_UML.Public
			   | SOME "private"   => XMI_UML.Private
			   | SOME "protected" => XMI_UML.Protected
			   | SOME "package"   => XMI_UML.Package
			   | NONE             => XMI_UML.Public
			   

fun getOrdering atts = case getAttValue "ordering" atts 
			of "unordered" => XMI_UML.Unordered
			 | "ordered"  => XMI_UML.Ordered

fun getAggregation atts = case getAttValue "aggregation" atts 
			   of "none" => XMI_UML.NoAggregation
			    | "aggregate" => XMI_UML.Aggregate
			    | "composite" => XMI_UML.Composite

fun getChangeability atts = case getAttValue "changeability" atts
			     of	"changeable" => XMI_UML.Changeable
			      | "frozen"     => XMI_UML.Frozen
			      | "addonly"    => XMI_UML.AddOnly

fun getKind atts = case getAttValue "kind" atts
		    of "in"     => XMI_UML.In
		     | "out"    => XMI_UML.Out
		     | "inout"  => XMI_UML.Inout
		     | "return" => XMI_UML.Return

fun getRange atts = ((valOf o Int.fromString o (getAttValue "lower")) atts, 
		     (valOf o Int.fromString o (getAttValue "upper")) atts)
    handle Option => error ("error in getRange")

(* not yet implemented: *)
fun skipOver string tree = if string = XmlTreeData.getElem tree 
			   then XmlTreeData.getTrees tree
			   else raise IllFormed ("skipOver "^string)
      
fun filterByName string trees = filter (fn x => string = XmlTreeData.getElem x) 
				       trees

fun findByName string trees = valOf (find (fn x => string = XmlTreeData.getElem x)
					  trees) 
    handle Option => error ("error in findByName "^string)

fun existsByName string trees = exists (fn x => string = XmlTreeData.getElem x) 
				       trees 

fun followByName string = (skipOver string) o (findByName string)
fun followAllByName string trees = map (skipOver string) 
				       (filterByName string trees) 


fun generic_tree2xmi name constructor tree = 
    if XmlTreeData.getElem tree = name
    then constructor (XmlTreeData.getAtts tree)
		     (XmlTreeData.getTrees tree)
    else raise IllFormed ("generic_tree2xmi "^name)

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

fun tree2aend tree           = generic_tree2xmi "UML:AssociationEnd" 
						mkAssociationEnd tree

fun mkAssociation atts trees = 
    { xmiid = getXmiId atts, name = getName atts,
      connection = map tree2aend (skipOver "UML:Association.connection" 
					   (hd trees)) }

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
	else raise IllFormed ("tree2oclexpression "^elem)
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

fun tree2operation tree      = generic_tree2xmi "UML:Operation" mkOperation tree

fun mkAttribute atts trees =
    { xmiid         = getXmiId atts,
      name          = getName atts,
      visibility    = getVisibility atts,
      changeability = getChangeability atts,
      type_id       = (getXmiIdref o XmlTreeData.getAtts o hd o 
		       (followByName "UML:StructuralFeature.type")) trees }

fun tree2attribute tree      = generic_tree2xmi "UML:Attribute" mkAttribute tree

fun mkUMLClass atts trees 
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

fun mkUMLPrimitive atts trees 
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
    
fun mkUMLEnumeration atts trees 
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

fun mkUMLVoid atts trees = XMI_UML.Void { xmiid = getXmiId atts, 
					  name = getName atts }

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
    
fun mkUMLCollection atts trees = XMI_UML.Collection (mkGenericCollection atts trees)
fun mkUMLSequence   atts trees = XMI_UML.Sequence   (mkGenericCollection atts trees)
fun mkUMLSet        atts trees = XMI_UML.Set        (mkGenericCollection atts trees)
fun mkUMLBag        atts trees = XMI_UML.Bag        (mkGenericCollection atts trees)
fun mkUMLOrderedSet atts trees = XMI_UML.OrderedSet (mkGenericCollection atts trees)

fun tree2classifier tree = 
    let val elem  = XmlTreeData.getElem tree
	val atts  = XmlTreeData.getAtts tree
	val trees = XmlTreeData.getTrees tree
    in 
	if elem = "UML:Class" then                          mkUMLClass atts trees
	else if elem = "UML:Primitive" orelse 
		elem = "UML:DataType" then              mkUMLPrimitive atts trees
	else if elem = "UML:Enumeration" then         mkUMLEnumeration atts trees
	else if elem = "UML15OCL.Types.VoidType" then        mkUMLVoid atts trees
	else if elem = "UML15OCL.Types.CollectionType" then 
	    mkUMLCollection atts trees
	else if elem = "UML15OCL.Types.SequenceType" then mkUMLSequence atts trees
	else if elem = "UML15OCL.Types.SetType" then           mkUMLSet atts trees
	else if elem = "UML15OCL.Types.BagType" then           mkUMLBag atts trees
	else if elem = "UML15OCL.Types.OrderedSetType" then
	    mkUMLOrderedSet atts trees
	else raise IllFormed ("tree2classifier "^elem)
    end


    
fun mkGeneralization atts trees =
    { xmiid     = getXmiId atts,
      child_id  = (getXmiIdref o XmlTreeData.getAtts o hd o
		   (followByName "UML:Generalization.child")) trees, 
      parent_id = (getXmiIdref o XmlTreeData.getAtts o hd o 
		   (followByName "UML:Generalization.parent")) trees }

fun tree2generalization tree = generic_tree2xmi "UML:Generalization" 
						mkGeneralization tree

    
fun tree2package tree = 
    if XmlTreeData.getElem tree = "UML:Model" orelse
       XmlTreeData.getElem tree = "UML:Package" then 
	let val trees = skipOver "UML:Namespace.ownedElement" 
				 ((hd o XmlTreeData.getTrees) tree)
	    val atts = XmlTreeData.getAtts tree in
	    XMI_UML.UMLPackage { xmiid                = getXmiId atts, 
			 name                 = getName atts,
			 visibility           = getVisibility atts,
			 ownedPackages        = (map tree2package 
						     (filterPackages trees)), 
			 ownedClassifiers     = (map tree2classifier
						     (filterClassifiers trees)),
			 ownedAssociations    = getAssociations trees, 
			 ownedGeneralizations = (map tree2generalization
						     (filterByName "UML:Generalization"
								   trees)),
			 ownedConstraints     = map tree2constraint (filterConstraints trees) }
	end
    else raise IllFormed "tree2package"
	

fun filterStereotypes trees = filterByName "UML:Stereotype" trees

fun mkStereotype atts trees = 
    { xmiid = getXmiId atts,
      name = getName atts
     }

fun tree2stereotype tree = generic_tree2xmi "UML:Stereotype" mkStereotype tree

fun filterStereotypes trees = filterByName "UML:Stereotype" trees

fun filterVariableDecs trees = filterByName "UML15OCL.Expressions.VariableDeclaration" trees

fun mkVariableDec atts trees = 
    { xmiid = getXmiId atts,
      name = getName atts,
      declaration_type = (getXmiIdref o XmlTreeData.getAtts o hd o 
		   (followByName "OCL.Expressions.VariableDeclaration.type")) trees
     }

fun tree2variable_dec tree = generic_tree2xmi "UML15OCL.Expressions.VariableDeclaration" mkVariableDec tree

fun mkXmiContent atts trees = 
    { packages = (map tree2package (filterPackages trees)),
      constraints = (map tree2constraint (filterConstraints trees)),
      classifiers = (map tree2classifier (filterClassifiers trees)),
      stereotypes = (map tree2stereotype (filterStereotypes trees)),
      variable_declarations = (map tree2variable_dec (filterVariableDecs trees))
      }


fun tree2xmicontent tree = generic_tree2xmi "XMI.content" mkXmiContent tree


fun findXmiContent tree = if XmlTreeData.getElem tree = "XMI.content" 
			  then [tree]
			  else List.concat (map findXmiContent 
						(XmlTreeData.getTrees tree))
			       
fun parseXMI filename = 
    let val trees = findXmiContent (ParseXmlTree.parseXmlTree filename)
    in
	tree2xmicontent (hd trees)
    end
	handle IllFormed msg => error msg
end


