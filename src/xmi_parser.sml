(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_parser.sml --- an xmi-parser for the import interface for su4sml
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007 ETH Zurich, Switzerland
 *               2008-2009 Achim D. Brucker, Germany
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)

structure XmiParser : sig
    val readFile: string -> XMI.XmiContent
end =
struct
open Rep_Helper
open XmlTree
open XmlTreeHelper

(* some syntax sugar *)
infix 1 |>
infix 2 is
val map_optional = Option.map
fun get_optional_or_default x y = Option.getOpt(y,x)

fun bool_value_of string atts = 
    let val att = value_of string atts
    in 
	(valOf o Bool.fromString) att 
	handle Option => Logger.error ("boolean attribute \""^string^
				"\" has non-boolean value \""^att^
                                "\" (xmi.id = "^(value_of "xmi.id" atts)^")")
    end


fun int_value_of string atts = 	  
    let val att = value_of string atts
    in 
	(valOf o Int.fromString) att 
	handle Option => Logger.error ("integer attribute \""^string^
				"\" has non-integer value \""^att^
                                "\" (xmi.id = "^(value_of "xmi.id" atts)^")")
    end
    
val language  = value_of "language" 
val body      = value_of "body" 
val xmiid     = value_of "xmi.id"    
val name      = value_of "name"      
fun xmiidref t = t |> attributes |> value_of "xmi.idref"
                 
fun optional_name_or_empty atts = atts |> optional_value_of "name"
                                       |> get_optional_or_default ""

fun unknown_attribute_value atts att s = Logger.error ("attribute \""^att^
                                                "\" has unknown value \""^s^
                                                "\" (xmi.id = "^(atts |> xmiid)^")")
                                         
fun visibility atts = 
    let val att = optional_value_of "visibility" atts 
    in
	case att of SOME "public"    => XMI.public
		  | SOME "private"   => XMI.private
		  | SOME "protected" => XMI.protected
		  | SOME "package"   => XMI.package
		  | NONE             => XMI.public
		  | SOME string      => unknown_attribute_value atts "visibility" string
    end
    

fun target_scope atts = 
    let val att = optional_value_of "targetScope" atts 
    in 
	case att of SOME "instance"   => XMI.InstanceScope
		  | SOME "classifier" => XMI.ClassifierScope
		  | NONE              => XMI.InstanceScope
		  | SOME s => unknown_attribute_value atts "targetScope" s
    end 
    
fun owner_scope atts = 
    let val att = optional_value_of "ownerScope" atts 
    in 
	case att of SOME "instance"    => XMI.InstanceScope
		  | SOME "classifier"  => XMI.ClassifierScope
		  | NONE               => XMI.InstanceScope
		  | SOME s => unknown_attribute_value atts "ownerScope" s
    end 

fun ordering default atts = 
    let val att = optional_value_of "ordering" atts 
    in 
	case att of SOME "unordered" => XMI.Unordered
		  | SOME "ordered"   => XMI.Ordered
		  | NONE             => default (* XMI.Unordered *)
		  | SOME s => unknown_attribute_value atts "ordering" s
    end 

fun aggregation atts = 
    let val att = optional_value_of "aggregation" atts 
    in
	    (case att of SOME "none"      => XMI.NoAggregation
		             | SOME "aggregate" => XMI.Aggregate
		             | SOME "composite" => XMI.Composite
		             | NONE             => XMI.NoAggregation
		             | SOME x => unknown_attribute_value atts "aggregation" x)
    end 

fun changeability atts = 
    let val att = optional_value_of "changeability" atts in
	case att of 
            SOME "changeable" => XMI.Changeable
	  | SOME "frozen"     => XMI.Frozen
	  | SOME "addonly"    => XMI.AddOnly
	  | NONE              => XMI.Changeable
	  | SOME x =>  unknown_attribute_value atts "changeability" x
    end 
    
fun kind atts = 
    let val att = atts |> optional_value_of "kind"
                       |> get_optional_or_default "inout"
    in
	case att of 
            "in"     => XMI.In
	  | "out"    => XMI.Out
	  | "inout"  => XMI.Inout
	  | "return" => XMI.Return
	  | x => unknown_attribute_value atts "kind" x
    end 
    
fun pseudo_state_kind atts = 
    let val att = value_of "kind" atts 
    in 	case att of "initial"  => XMI.initial
		  | "deep"     => XMI.deep
		  | "shallow"  => XMI.shallow
		  | "join"     => XMI.join
		  | "fork"     => XMI.fork
		  | "junction" => XMI.junction
		  | "choice"   => XMI.choice
		  | x          => unknown_attribute_value atts "kind (PseudoStateKind)" x
    end

fun mkRange tree = 
    let val atts = tree |> assert "UML:MultiplicityRange" |> attributes
    in 
        (int_value_of "lower" atts, int_value_of "upper" atts)
    end

fun mkMultiplicity tree = 
    assert "UML:Multiplicity" tree
           |> get "UML:Multiplicity.range"
           |> map mkRange

(* find the xmi.idref attribute of an element pointed to by name *)
fun xmiidref_to name tree = tree |> get_one name
                                 |> xmiidref

(* find the type of an OCl sub-expression *)
fun expression_type tree = tree |> xmiidref_to "OCL.Expressions.OclExpression.type" 
    handle _ => "DummyT"
(* hack: return a reference to a dummy*)
(* type if the real type is not found *)

(* this is a hack. This will still throw an exception in xmi2mdr, because the  *)
(* expression_type should be the xmiid of oclLib.Boolean, which we do not know *)
val triv_expr = XMI.LiteralExp {symbol = "true", 
				expression_type = "bool" }
                
(* FIX: this is only a dummy implementation *)
fun mkCollectionLiteralPart x = (xmiidref x)
                                
fun mkLiteralExp string tree = 
    XMI.LiteralExp 
        { symbol          = tree |> attributes |> value_of string,
	        expression_type = tree |> expression_type 
        }
    
fun mkOCLExpression (tree as Node(("UML15OCL.Expressions.BooleanLiteralExp",
                                   atts),_)) =
    mkLiteralExp "booleanSymbol" tree
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.IntegerLiteralExp",
                                   atts),_)) =
    mkLiteralExp "integerSymbol" tree
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.StringLiteralExp",
                                   atts),_)) = 
    mkLiteralExp  "stringSymbol" tree
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.RealLiteralExp",
                                   atts),_)) = 
    mkLiteralExp "realSymbol" tree
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.CollectionLiteralExp",
                                   atts),_))
    = XMI.CollectionLiteralExp 
          { parts = nil, 
  (* map mkCollectionLiteralPart (follow "OCL.Expressions.\
                                         \CollectionLiteralExp.parts" trees),*)
	              expression_type = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.OperationCallExp",
                                   atts),_))
    = XMI.OperationCallExp 
	        { source    = (tree |> get_one "OCL.Expressions.\
                                         \PropertyCallExp.source"
                              |> mkOCLExpression)
     (* This hack is necessary to support TYPE::allInstances() as parsed *)
            (* by dresden-ocl. *)
            handle ex => 
                   XMI.LiteralExp
                       { symbol = "",
                         expression_type = tree |> get_one "OCL.Expressions.\
                                                           \FeatureCallExp.\
                                                           \srcType"
                                                |> xmiidref
                       },
	          arguments = tree |> get "OCL.Expressions.OperationCallExp.arguments"
                             |> map mkOCLExpression,
	          referredOperation = tree |> xmiidref_to "OCL.Expressions.\
                                                    \OperationCallExp.\
                                                    \referredOperation",
	          expression_type   = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.OclOperationWith\
                                   \TypeArgExp",atts),_))
    = XMI.OperationWithTypeArgExp
	        { source       = tree |> get_one "OCL.Expressions.PropertyCallExp.\
                                           \source"
                                |> mkOCLExpression,
	          name         = atts |> name,
	          typeArgument = tree |> xmiidref_to "OCL.Expressions.OclOperation\
                                               \WithTypeArgExp.typeArgument",
	          expression_type = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.AttributeCallExp",
                                   atts),_))
    = XMI.AttributeCallExp 
	        { source            = tree |> get_one "OCL.Expressions.PropertyCall\
                                                \Exp.source"
                                     |> mkOCLExpression,
	          referredAttribute = tree |> xmiidref_to "OCL.Expressions.Attribute\
                                                    \CallExp.referred\
                                                    \Attribute",
	          expression_type   = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.AssociationEndCall\
                                   \Exp",atts),_))
    = XMI.AssociationEndCallExp 
	        { source = tree |> get_one "OCL.Expressions.PropertyCallExp.source"
                          |> mkOCLExpression,
	          referredAssociationEnd  = tree |> xmiidref_to 
			                                     "OCL.Expressions.AssociationEndCall\
                                           \Exp.referredAssociationEnd",
	          expression_type         = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.AssociationClassCall\
                                   \Exp",atts),_))
    =  Logger.error ("AssociationClassCallExp is not yet implemented"^some_id tree)
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.VariableExp",atts),_))
    = XMI.VariableExp 
          { referredVariable = tree |> xmiidref_to
				                            "OCL.Expressions.VariableExp.referred\
                                    \Variable",
	          expression_type  = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.IfExp",atts),_))
    = XMI.IfExp 
          { condition       = tree |> get_one "OCL.Expressions.IfExp.condition"
                                   |> mkOCLExpression,
	          thenExpression  = tree |> get_one "OCL.Expressions.IfExp.then\
                                              \Expression"
                                   |> mkOCLExpression,
	          elseExpression  = tree |> get_one "OCL.Expressions.IfExp.else\
                                              \Expression"
                                   |> mkOCLExpression,
	          expression_type = tree |> expression_type }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.LetExp",atts),_)) 
    = XMI.LetExp 
	        { variable =  let val vard = tree |> get_one "OCL.Expressions.Let\
                                                       \Exp.variable"
                          val atts = vard |> attributes
                        in 
                          { xmiid            = atts |> xmiid,
			                      name             = atts |> name, 
			                      declaration_type = vard |> xmiidref_to 
				                                            "OCL.Expressions.Variable\
                                                    \Declaration.type",
			                      init = vard |> get_one
                                        "OCL.Expressions.VariableDeclaration.\
                                        \initExpression"
                                        |> mkOCLExpression
                                        |> SOME
                          }
                        end,
	          inExpression    = tree |> get_one "OCL.Expressions.LetExp.in"
                                   |> mkOCLExpression,
	          expression_type = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.IterateExp",atts),_))
    = XMI.IterateExp 
          { result    = tree |> get_one "OCL.Expressions.IterateExp.result"
                             |> mkVariableDec,
	          iterators = tree |> get_many "OCL.Expressions.LoopExp.iterators" 
                             |> map mkVariableDec,
	          body      = tree |> get_one "OCL.Expressions.LoopExp.body"
                             |> mkOCLExpression,
	          source    = tree |> get_one "OCL.Expressions.PropertyCallExp.\
                                        \source"
                             |> mkOCLExpression,
	          expression_type = tree |> expression_type 
          }
  | mkOCLExpression (tree as Node(("UML15OCL.Expressions.IteratorExp",atts),_)) 
    = XMI.IteratorExp 
          { name      = atts |> name,
	          iterators = tree |> get_many "OCL.Expressions.LoopExp.iterators" 
                             |> map mkVariableDec,
	          body      = tree |> get_one "OCL.Expressions.LoopExp.body"
                             |> mkOCLExpression,
	          source    = tree |> get_one "OCL.Expressions.PropertyCallExp.source"
                             |> mkOCLExpression,
	          expression_type = tree |> expression_type 
          }	  
  | mkOCLExpression tree = 
    Logger.error ("unknown OCLExpression type \""^(tagname tree)^"\""^some_id tree^
           ".")
and mkVariableDec vtree = 
    let val atts = vtree |> assert "UML15OCL.Expressions.VariableDeclaration"
                         |> attributes 
    in
	    { xmiid = atts |> xmiid,
	      name  = atts |> name,
	      init  = vtree |> get_optional "OCL.Expressions.VariableDeclaration.\
                                      \initExpression"
                      |> map_optional mkOCLExpression,
	      declaration_type = vtree |> get_one "OCL.Expressions.Variable\
                                            \Declaration.type" 
                                 |> xmiidref 
	    }
    end
        (* handle IllFormed msg => Logger.error ("in mkVariableDec: "^msg)*)
 
fun mkTaggedValue tree =
    let val atts = tree |> assert "UML:TaggedValue" |> attributes
    in
        { xmiid    = atts |> xmiid,
          dataValue= tree |> find_child "UML:TaggedValue.dataValue"
                          |> children 
                          |> map text
                          |> String.concat,
          tag_type = tree |> get_one "UML:TaggedValue.type"
                          |> assert "UML:TagDefinition"
                          |> xmiidref
	}
    end
(*handle IllFormed msg => Logger.error ("in mkTaggedValue: "^msg)*)

fun mkAttribute tree = 
    let val atts = tree |> assert "UML:Attribute" |> attributes
    in
	{ xmiid         = atts |> xmiid,
	  name          = atts |> name,
	  visibility    = atts |> visibility,
	  changeability = atts |> changeability,
	  ordering      = atts |> ordering XMI.Ordered, 
	  initialValue  = tree |> get_optional "UML:Attribute.initialValue"
                         |> map_optional (get_optional 
                                              "OCL.Expressions.\
                                              \ExpressionInOcl.bodyExpression")
                         |> Option.join
                         |> map_optional mkOCLExpression,
	  type_id       = tree |> get_optional "UML:StructuralFeature.type"
                         |> map_optional xmiidref
                         |> get_optional_or_default "",
	  multiplicity  = tree |> get_optional "UML:StructuralFeature.multiplicity" 
                         |> map_optional mkMultiplicity
                         |> get_optional_or_default [(1,1)],
	  targetScope   = atts |> target_scope,
	  ownerScope    = atts |> owner_scope,
	  stereotype    = tree |> get "UML:ModelElement.stereotype"
                         |> map xmiidref ,
	  taggedValue   = tree |> get "UML:ModelElement.taggedValue" 
                         |> map mkTaggedValue 
  }
    end
(*handle IllFormed msg => Logger.error ("in mkAttribute: "^msg)*)

fun mkQualifier tree =
    get_maybe "UML:Attribute" tree
              |> map mkAttribute

fun mkAssociationEnd association  tree:XMI_Core.AssociationEnd = 
    let val atts = tree |> assert "UML:AssociationEnd" |> attributes
    in 
	    { xmiid          = atts |> xmiid, 
	      name           = atts |> optional_value_of "name", 
	      association    = association,
	      isNavigable    = atts |> bool_value_of "isNavigable" ,
	      ordering       = atts |> ordering XMI.Unordered,
	      aggregation    = atts |> aggregation,
	      targetScope    = atts |> target_scope,
	      multiplicity   = tree |> get_optional "UML:AssociationEnd.\
                                              \multiplicity"  
                              |> map_optional mkMultiplicity
                              |> get_optional_or_default [(0,~1)],
	      changeability  = atts |> changeability, 
        qualifier      = tree |> get_optional "UML.AssociationEnd.qualifier"
                              |> map_optional mkQualifier
                              |> get_optional_or_default [],
	      visibility     = atts |> visibility,
	      participant_id = tree |> get_one "UML:AssociationEnd.participant"
                              |> xmiidref
      }
    end
(*handle IllFormed msg => Logger.error ("in mkAssociationEnd: "^msg)*)

(* This is a hack to handle the implicit association end to *)
(* the AssociationClass itself. *) 
(* update: no longer needed *)
fun mkAssociationEndFromAssociationClass association tree :XMI.AssociationEnd =
    let val atts = tree |> assert "UML:AssociationClass" |> attributes
    in 
        {(* xmiids are used as keys in a lookup table. *)
         (* to avoid name-clashes with the xmiid for the *)
         (* class itsel, we simply add a suffix *) 
         xmiid          = (atts |> xmiid)^"_aend", 
         (* rep_parser already takes care of naming the association end *)
	           name           = NONE,
	       association    = association,
	       isNavigable    = true,
	       ordering       = XMI_DataTypes.Unordered,
	       aggregation    = XMI_DataTypes.Aggregate,
	       targetScope    = XMI_DataTypes.InstanceScope,
	       multiplicity   = [(0,~1)] (* FIX: is this always the correct multiplicity? *),
         qualifier      = [],
	       changeability  = XMI_DataTypes.Changeable, 
	       visibility     = XMI_DataTypes.public,
	       participant_id = atts |> xmiid
	      }
    end

(* FIX: this is a hack to handle AssociationClasses.                 *)
(* From an AssociationClass,  we build the corresponding association *)
(* that will later be handles just like any other association.       *)
fun mkAssociationFromAssociationClass tree =
    let 
	val atts = tree |> assert "UML:AssociationClass" |> attributes
	val id = atts |> xmiid
    in 
        { xmiid      = id, 
	  name       = atts |> optional_value_of "name" ,
	  connection = tree |> get_many "UML:Association.connection"
                            |> map (mkAssociationEnd id)
	}
    end
(*handle IllFormed msg => Logger.error ("in mkAssociation: "^msg)*)


fun mkAssociation tree = 
    let 
	    val _ = Logger.debug2 ("XmiParser.mkAssociation\n")
	    val atts = tree |> assert "UML:Association" |> attributes
	    val   id = atts |> xmiid
	    (* FIXME: empty string is returned as (SOME "") instead of NONE *)
	    val name_tmp = atts |> optional_value_of "name"
	    val name = if (isSome name_tmp) andalso ((valOf name_tmp) = "")
		             then NONE
		             else name_tmp
	    val res = { xmiid      = id,  
			name       = name,
			connection = tree |> get_many "UML:Association.connection" 
					  |> map (mkAssociationEnd id)
		      }
	    val _ = Logger.debug2 ("end XmiParser.mkAssociation")
    in
	res
    end
(* handle IllFormed msg => Logger.error ("in mkAssociation: "^msg)*)
    
val filterAssociations = filter "UML:Association"
val filterAssociationClasses = filter "UML:AssociationClass"

fun filterConstraints trees = List.filter (fn x => (tagname o (get_one "UML:Constraint.body")) x 
						   = "UML15OCL.Expressions.ExpressionInOcl") 
					  (filter "UML:Constraint" trees)
                              
val filterStereotypes   = filter "UML:Stereotype" 
val filterVariableDecs  = filter "UML15OCL.Expressions.VariableDeclaration" 
val filterPackages      = fn trees => append (filter "UML:Package" trees)
				             (filter "UML:Model" trees)			      
val filterStateMachines = filter "UML:StateMachine" 
val filterActivityGraphs= filter "UML:ActivityGraph" 
val filterEvents        = fn x => append (filter "UML:CallEvent" x)
				         (filter "UML:SignalEvent" x)(* add SignalEvents? *)

(* there may be other kinds of dependencies, but we do not parse them atm *)
val filterDependencies = filter "UML:Abstraction" 

val filterTagDefinitions = filter "UML:TagDefinition" 

(* FIX: other classifiers *) 
fun filterClassifiers trees = 
    List.filter (fn x => let val elem = tagname x in
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
    let val atts = tree |> assert "UML:Abstraction" |> attributes
    in  
	{ xmiid      = atts |> xmiid,
	  client     = tree |> get_one "UML:Dependency.client" 
                            |> xmiidref,
	  supplier   = tree |> get_one "UML:Dependency.supplier" 
                            |> xmiidref,
	  stereotype = tree |> get_one "UML:ModelElement.stereotype" 
                            |> xmiidref 
        }
    end
(*handle IllFormed msg => Logger.error ("in mkDependency: "^msg) *)

fun mkConstraint tree = 
    let val atts = tree |> assert "UML:Constraint" |> attributes
    in 
        { xmiid = atts |> xmiid,
	  name  = atts |> optional_value_of "name",
	  constraint_type = tree |> get_one "UML:ModelElement.stereotype" 
                                 |> xmiidref,
	  body = tree |> get_one "UML:Constraint.body"
                      |> assert  "UML15OCL.Expressions.ExpressionInOcl"
                      |> get_one "OCL.Expressions.ExpressionInOcl.bodyExpression"
                      |> mkOCLExpression 
	}
    end
(*handle IllFormed msg => Logger.error ("in mkConstraint: "^msg)*)


fun mkParameter tree = 
    let val atts = tree |> assert "UML:Parameter" |> attributes
    in
        { xmiid   = atts |> xmiid,
	  name    = atts |> name,
	  kind    = atts |> kind,
	  type_id = tree |> get_optional "UML:Parameter.type" 
			 |> map_optional xmiidref 
                         |> get_optional_or_default ""
        }
    end
(*handle IllFormed msg => Logger.error ("in mkParameter: "^msg)*)

fun mkOperation tree = 
    let val atts = tree |> assert "UML:Operation" |> attributes
    in
	{ xmiid         = atts |> xmiid,
	  name          = atts |> name, 
	  visibility    = atts |> visibility,
	  isQuery       = atts |> bool_value_of "isQuery",
	  ownerScope    = atts |> owner_scope,
	  parameter     = tree |> get "UML:BehavioralFeature.parameter" 
			       |> map mkParameter,
	  constraints   = tree |> get_maybe "UML:ModelElement.constraint" 
                               |> map xmiidref
	}
    end
(*handle IllFormed msg =>  Logger.error ("in mkOperation: "^msg)*)

fun mkTagDefinition tree =
    let val atts = tree |> assert "UML:TagDefinition" |> attributes
    in 
        { xmiid        = atts |> xmiid,
	  name         = atts |> name,
	  multiplicity = tree |> get_one "UML:TagDefinition.multiplicity"
                              |> mkMultiplicity 
        }
    end
(*handle IllFormed msg => Logger.error ("in mkTagDefinition: "^msg)*)

fun mkStereotypeR tree = 
    let val atts = tree |> assert "UML:Stereotype" |> attributes
    in
        tree |> xmiidref
    end 
(*handle IllFormed msg => Logger.error ("in mkStereotype: "^msg)*)

fun mkAction tree = 
    let val atts      = tree |> attributes
        val expr      = tree |> get_one "UML:Action.script"
	val expr_atts = expr |> attributes 
    in
	XMI.mk_Procedure 
            { xmiid           = atts |> xmiid,
	      name            = atts |> optional_name_or_empty,
	      isSpecification = atts |> bool_value_of "isSpecification" ,
	      isAsynchronous  = atts |> bool_value_of "isAsynchronous" ,
	      language        = expr_atts |> language,
	      body            = expr_atts |> body ,
	      expression      = "" (* FIXME: is this even useful? *)}
    end
(*handle IllFormed msg => Logger.error ("in mkAction: "^msg)*)
    
(* This works for ArgoUML, i.e. 1.4 metamodels... *)
fun mkProcedure tree = 
    let val elem  = tagname    tree
    in 
	if elem = "UML:CallAction"      orelse
	   elem = "UML:CreateAction"    orelse
	   elem = "UML:DestroyAction"   orelse
           elem = "UML:ReturnAction"    orelse
           elem = "UML:SendAction"      orelse
           elem = "UML:TerminateAction" orelse
	   elem = "UML:UninterpretedAction" 
        then mkAction tree
	else Logger.error ("unknown Action type \""^elem^"\""^(some_id tree)^".")
    end

fun mkGuard tree = 
    let val atts      = tree |> assert "UML:Guard" 
                             |> attributes
        val expr      = tree |> get_one "UML:Guard.expression" 
        val expr_atts = expr |> attributes 
    in
        XMI.mk_Guard
            { xmiid           = atts |> xmiid,
              name            = atts |> optional_name_or_empty,
              isSpecification = atts |> bool_value_of "isSpecification",
              visibility      = atts |> visibility,
              language        = if expr is "UML15OCL:Expressions.ExpressionInOcl" orelse
                                   expr is "UML:BooleanExpression"
				then expr_atts |> language 
                                else
                                    Logger.error ("unknown expression type \""^(tagname expr)^
                                           "\""^some_id expr^"."),
              body            = if expr is "UML:BooleanExpression" then
                                    SOME (expr_atts |> body)
                                else NONE,
              expression      = if expr is "UML15OCL:Expressions.ExpressionInOcl" 
				then SOME (mkOCLExpression expr)
				else NONE}
    end
(*handle IllFormed msg => Logger.error ("in mkGuard: "^msg)*)


fun mkTransition tree = 
    let val atts = tree |> assert "UML:Transition" |> attributes 
    in 
        XMI.mk_Transition  
            { xmiid           = atts |> xmiid,
              isSpecification = atts |> bool_value_of "isSpecification",
              source          = tree |> get_one "UML:Transition.source"
                                     |> xmiidref,
              target          = tree |> get_one "UML:Transition.target"
                                     |> xmiidref,
	      guard           = tree |> get_optional "UML:Transition.guard"
                                     |> map_optional mkGuard,
	      trigger         = tree |> get_optional "UML:Transition.trigger"
                                     |> map_optional xmiidref,
	      effect          = tree |> get_optional "UML:Transition.effect"
                                     |> map_optional mkProcedure,
              taggedValue     = tree |> get "UML:ModelElement.taggedValue"
                                     |> map mkTaggedValue
            }
    end
(*handle IllFormed msg => Logger.error ("in mkTransition: "^msg)*)
    


fun mkState tree =
    let val elem              = tagname    tree
	val atts              = attributes tree
        val xmiid             = atts |> xmiid
        val name              = atts |> optional_name_or_empty
        val isSpecification   = atts |> bool_value_of "isSpecification"
        fun idref tree        = tree |> xmiidref
        val stereotypes       = tree |> get "UML:ModelElement.stereotype"
                                     |> map mkStereotypeR
        val incoming          = tree |> get "UML:StateVertex.incoming"
                                     |> map xmiidref
        val outgoing          = tree |> get "UML:StateVertex.outgoing"
                                     |> map xmiidref
        fun getSubvertex tree = tree |> get_many "UML:CompositeState.subvertex"
                                     |> map mkState
        val entry             = tree |> get_optional "UML:State.entry"
                                     |> map_optional mkProcedure
        val exit              = tree |> get_optional "UML:State.exit"
                                     |> map_optional mkProcedure
        val do_act            = tree |> get_optional "UML:State.doActivity"
                                     |> map_optional mkProcedure
        val tagval            = tree |> get "UML:ModelElement.taggedValue"
                                     |> map mkTaggedValue
	fun getType tree      = tree |> get_one "UML:ObjectFlowState.type"
                                     |> xmiidref
    in case elem of 
           "UML:CompositeState" => 
           XMI.CompositeState
               { xmiid=xmiid,name=name,isSpecification=isSpecification,
                 stereotype   = stereotypes,
                 isConcurrent = atts |> bool_value_of "isConcurrent",
                 outgoing     = outgoing, incoming = incoming, 
	         subvertex    = getSubvertex tree,
                 entry        = entry,
                 exit         = exit,
                 doActivity   = do_act,
                 taggedValue  = tagval
               }
         |"UML:SubactivityState" => 
          XMI.SubactivityState
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                isConcurrent = atts |> bool_value_of "isConcurrent",
                isDynamic    = atts |> bool_value_of "isDynamic",
                outgoing     = outgoing, incoming = incoming, 
	        subvertex    = getSubvertex tree,
                entry        = entry,
                exit         = exit,
                doActivity   = do_act,
                submachine   = mkStateMachine tree,
                (* HACK ! So far, no UML tool supports this. *)
                (* Parser has to be adapted when we find a first example ... *)
                taggedValue  = tagval}
         |"UML:ActionState" => 
          XMI.ActionState 
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                outgoing     = outgoing, incoming = incoming, 
                isDynamic    = atts |> bool_value_of "isDynamic",
                entry        = entry,
                taggedValue  = tagval}
         |"UML:Pseudostate" => 
          XMI.PseudoState 
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                kind         = atts |> pseudo_state_kind,
                outgoing     = outgoing,incoming = incoming,
                taggedValue  = tagval}
         |"UML:SimpleState" => 
          XMI.SimpleState
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                entry        = entry,
                exit         = exit,
                doActivity   = do_act,
                outgoing     = outgoing, incoming = incoming,
                taggedValue  = tagval}
         |"UML:ObjectFlowState" => 
          XMI.ObjectFlowState
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                entry        = entry,
                exit         = exit,
                doActivity   = do_act,
                outgoing     = outgoing, incoming = incoming, 
                isSynch      = atts |> bool_value_of "isSynch",
                parameter    = nil,
                type_        = tree |> getType,
                taggedValue  = tagval}
         |"UML:FinalState" => 
          XMI.FinalState
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                entry        = entry,
                exit         = exit,
                doActivity   = do_act,
                incoming = incoming,
                taggedValue  = tagval}
         |"UML:SyncState" => 
          XMI.SyncState
              { xmiid=xmiid,name=name,isSpecification=isSpecification,
                stereotype   = stereotypes,
                bound        = 0,
                outgoing     = outgoing,incoming = incoming,
                taggedValue  = tagval}
          
         | s => Logger.error ("unknown StateVertex type \""^s^"\""^some_id tree^".")
    end
and mkStateMachine tree =
    let val atts = tree |> assert "UML:StateMachine" |> attributes 
    in 
        XMI.mk_StateMachine 
            { isSpecification = atts |> bool_value_of "isSpecification",
              xmiid           = atts |> xmiid, 
              contextxmiid    = tree |> get_one "UML:StateMachine.context"
                                     |> xmiidref, 
              top             = tree |> get_one "UML:StateMachine.top"
                                     |> mkState,
              transitions     = tree |> get "UML:StateMachine.transitions"
                                     |> map mkTransition
            }
    end
(*handle IllFormed msg => Logger.error ("in mkStateMachine: "^msg)*)


fun mkActivityGraph tree =
    let val atts = tree |> assert "UML:ActivityGraph" |> attributes
    in
        XMI.mk_ActivityGraph 
            { isSpecification = atts |> bool_value_of "isSpecification",
              xmiid           = atts |> xmiid, 
              contextxmiid    = tree |> get_one "UML:StateMachine.context"
                                     |> xmiidref, 
              top             = tree |> get_one "UML:StateMachine.top"
                                     |> mkState,
              transitions     = tree |> get "UML:StateMachine.transitions"
                                     |> map mkTransition,
              partition       = nil}
    end
(*handle IllFormed msg => Logger.error ("in mkActivityGraph: "^msg)*)
    
fun mkClass atts tree = 
    let
	val _ = Logger.debug2 ("XmiParser.mkClass \n")
	val res = XMI.Class 
            { xmiid              = atts |> xmiid,
	      name               = atts |> name,
	      isActive           = atts |> bool_value_of "isActive",
	      visibility         = atts |> visibility,
	      isLeaf             = atts |> bool_value_of "isLeaf",
	      generalizations    = tree |> get "UML:GeneralizableElement.generalization" 
					|> map xmiidref,
	      attributes         = tree |> get "UML:Classifier.feature"
					|> filter "UML:Attribute"
					|> map mkAttribute,
	      operations         = tree |> get "UML:Classifier.feature"
					|> filter "UML:Operation"
					|> map mkOperation,
	      invariant          = tree |> get "UML:ModelElement.constraint" 
					|> map xmiidref,
	      stereotype         = tree |> get "UML:ModelElement.stereotype" 
					|> map xmiidref,
	      taggedValue        = tree |> get "UML:ModelElement.taggedValue" 
					|> map mkTaggedValue,
	      clientDependency   = tree |> get "UML:ModelElement.clientDependency"
					|> map xmiidref,
	      supplierDependency = tree |> get "UML:ModelElement.supplierDependency"
					|> map xmiidref,
	      classifierInState  = tree |> get "UML:Namespace.ownedElement"
					|> filter "UML:ClassifierInState"
					|> map (xmiid o attributes),
	      state_machines     = tree |> get "UML:Namespace.ownedElement"
					|> filter "UML:StateMachine"
					|> map mkStateMachine,
	      activity_graphs    = tree |> get "UML:Namespace.ownedElement" 
					|> filter "UML:ActivityGraph"
					|> map mkActivityGraph 
            }
	val _ = Logger.debug2 ("end XmiParser.mkClass \n")
    in
	res
    end
(*handle IllFormed msg => Logger.error ("Error in mkClass "^(name atts)^
				 ": "^msg)*)
    
(* extended to match Rep.AssociationClass *)
fun mkAssociationClass atts tree =
    let
	    val _ = Logger.debug2 ("XmiParser.mkAssociationClass\n")
	    val id = atts |> xmiid
	    val res = XMI.AssociationClass
			  { xmiid              = id,
			    name               = atts |> name,
			    isActive           = atts |> bool_value_of "isActive",
			    visibility         = atts |> visibility,
			    isLeaf             = atts |> bool_value_of "isLeaf",
			    generalizations    = tree |> get "UML:GeneralizableElement.\
							     \generalization" 
						      |> map xmiidref,
			    attributes         = tree |> get "UML:Classifier.feature"
				                      |> filter "UML:Attribute"
						      |> map mkAttribute,
			    operations         = tree |> get "UML:Classifier.feature"
				                      |> filter "UML:Operation"
						      |> map mkOperation,
			    invariant          = tree |> get "UML:ModelElement.constraint" 
				                      |> map xmiidref,
			    stereotype         = tree |> get "UML:ModelElement.stereotype" 
				                      |> map xmiidref,
			    taggedValue        = tree |> get "UML:ModelElement.taggedValue" 
				                      |> map mkTaggedValue,
			    clientDependency   = tree |> get "UML:ModelElement.client\
							     \Dependency"
				                      |> map xmiidref,
			    supplierDependency = tree |> get "UML:ModelElement.supplier\
							     \Dependency"
				                      |> map xmiidref,
			    (*classifierInState  = tree |> get "UML:Namespace.ownedElement"
					                |> filter "UML:ClassifierInState"
					                |> map (xmiid o attributes),
			     state_machines     = tree |> get "UML:Namespace.ownedElement"
					               |> filter "UML:StateMachine"
					               |> map mkStateMachine,                               activity_graphs    = tree |> get "UML:Namespace.ownedElement"
																      |> filter "UML:ActivityGraph"
																      |> map mkActivityGraph,
			     *)connection         = tree |> get_many "UML:Association.connection" 
					                 |> map (mkAssociationEnd id)
			  }
	    val _ = Logger.debug2 ("end XmiParser.mkAssociation Class\n")
    in
	res
    end

(*handle IllFormed msg => Logger.error ("in mkAssociationClass: "^msg)*)


fun mkPrimitive atts tree
  = XMI.Primitive 
        { xmiid      = atts |> xmiid,
		  name       = atts |> name,
		  operations = tree |> get "UML:Classifier.feature"
							|> filter "UML:Operation"
                            |> map mkOperation,
		  generalizations = tree |> get "UML:GeneralizableElement.generalization" 
                                 |> map xmiidref,
		  invariant       = tree |> get "UML:ModelElement.constraint" 
								 |> map xmiidref,
          taggedValue     = tree |> get "UML:ModelElement.taggedValue"
                                 |> map mkTaggedValue
		  }
(* handle IllFormed msg => Logger.error ("in mkPrimitive: "^msg)*)

fun mkInterface atts tree
  = XMI.Interface 
        { xmiid              = atts |> xmiid,
	  name               = atts |> name,
	  operations         = tree |> get "UML:Classifier.feature"
				    |> filter "UML:Operation"
                                    |> map mkOperation,
	  generalizations    = tree |> get "UML:GeneralizableElement.generalization" 
                                    |> map xmiidref,
	  invariant          = tree |> get "UML:ModelElement.constraint" 
				    |> map xmiidref,
	  clientDependency   = tree |> get "UML:ModelElement.clientDependency"
				    |> map xmiidref,
	  supplierDependency = tree |> get "UML:ModelElement.supplierDependency"
				    |> map xmiidref
	}
(*handle IllFormed msg => Logger.error ("in mkInterface: "^msg)*)

fun mkEnumerationLiteral tree =
    tree |> assert "UML:EnumerationLiteral" 
         |> attributes |> name
(*handle IllFormed msg =>  Logger.error ("in mkOperation: "^msg)*)


fun mkEnumeration atts tree
  = XMI.Enumeration 
        { xmiid              = atts |> xmiid,
	  name               = atts |> name,
	  operations         = tree |> get "UML:Classifier.feature"
				    |> filter "UML:Operation"
                                    |> map mkOperation,
	  generalizations    = tree |> get "UML:GeneralizableElement.generalization" 
                                    |> map xmiidref,
	  invariant          = tree |> get "UML:ModelElement.constraint" 
				    |> map xmiidref,
	  literals           = tree |> get "UML:Enumeration.literal"
                                    |> map mkEnumerationLiteral
        }
(*    handle IllFormed msg => Logger.error ("in mkEnumeration: "^msg)*)
    
fun mkVoid atts tree = XMI.Void { xmiid = atts |> xmiid, 
				  name  = atts |> name 
                                }
(* handle IllFormed msg => Logger.error ("in mkVoid: "^msg)*)
                       

fun mkGenericCollection atts tree = 
    { xmiid              = atts |> xmiid,
      name               = atts |> name,
      operations         = tree |> get "UML:Classifier.feature"
				|> filter "UML:Operation"
                                |> map mkOperation,
      generalizations    = tree |> get "UML:GeneralizableElement.generalization" 
                                |> map xmiidref,
      elementtype        = tree |> get_one "OCL.Types.CollectionType.elementType" 
			        |> xmiidref
    }
(* handle IllFormed msg => Logger.error ("in mkGenericCollection: "^msg) *)

    
fun mkCollection atts tree = XMI.Collection (mkGenericCollection atts tree)
fun mkSequence   atts tree = XMI.Sequence   (mkGenericCollection atts tree)
fun mkSet        atts tree = XMI.Set        (mkGenericCollection atts tree)
fun mkBag        atts tree = XMI.Bag        (mkGenericCollection atts tree)
fun mkOrderedSet atts tree = XMI.OrderedSet (mkGenericCollection atts tree)

fun mkStereotype tree = 
    let val atts = tree |> assert "UML:Stereotype" |> attributes
    in
        { xmiid                = atts |> xmiid,
	  name                 = atts |> name,
	  baseClass            = Option.map (text o hd o text_children) 
                                            (find_some "UML:Stereotype.baseClass" 
                                                       (node_children tree)),
	  stereotypeConstraint = NONE  (* FIXME, not supported by ArgoUML 0.22 *)
	}
    end 
(*    handle IllFormed msg => Logger.error ("in mkStereotype: "^msg)*)


fun mkClassifier tree = 
    let val elem  = tagname    tree
	val atts  = attributes tree
	val trees = node_children    tree
    in 
	case elem of "UML:Class"                     => mkClass atts tree
		   | "UML:AssociationClass"          => mkAssociationClass atts tree
		   | "UML:Interface"                 => mkInterface atts tree
		   | "UML:DataType"                  => mkPrimitive atts tree
		   | "UML:Primitive"                 => mkPrimitive atts tree
		   | "UML:Enumeration"               => mkEnumeration atts tree
		   | "UML15OCL.Types.VoidType"       => mkVoid atts tree
		   | "UML15OCL.Types.CollectionType" => mkCollection atts tree
		   | "UML15OCL.Types.SequenceType"   => mkSequence atts tree
		   | "UML15OCL.Types.SetType"        => mkSet atts tree
		   | "UML15OCL.Types.BagType"        => mkBag atts tree
		   | "UML15OCL.Types.OrderedSetType" => mkOrderedSet atts tree
		   | _ => Logger.error ("unknown Classifier type \""^elem^
                                 "\""^some_id tree^".")
    end
    

    
fun mkGeneralization tree = 
    let val atts = tree |> assert "UML:Generalization" |> attributes
    in
	{ xmiid     = atts |> xmiid,
	  child_id  = tree |> get_one "UML:Generalization.child"
                           |> xmiidref, 
	  parent_id = tree |> get_one "UML:Generalization.parent"
                           |> xmiidref
        }
    end


fun mkCallEvent atts tree =
    XMI.CallEvent 
        { xmiid     = atts |> xmiid,
	  name      = atts |> optional_name_or_empty,
	  operation = tree |> get_one "UML:CallEvent.operation"
                           |> xmiidref,
	  parameter = tree |> get "UML:Event.parameter" 
			   |> map mkParameter 
	}

fun mkSignalEvent atts tree = XMI.SignalEvent { xmiid     = atts |> xmiid,
                                                name      = atts |> optional_name_or_empty,
                                                parameter = tree |> get "UML:Event.parameter"
                                                                 |> map mkParameter
                                              }
                              
(* FIXME: other events ? *)
fun mkEvent tree = 
    let val elem  = tagname    tree
	val atts  = attributes tree
    in 
	case elem of "UML:CallEvent"   => mkCallEvent atts tree
		   | "UML:SignalEvent" => mkSignalEvent atts tree
		   | _ => Logger.error ("unknown Event type \""^elem^"\""^some_id tree^".")
    end




fun mkPackage tree = 
    if tree is "UML:Model" orelse tree is "UML:Package" 
    then let val trees = tree |> get "UML:Namespace.ownedElement"
	     val atts = attributes tree 
             val package_name = atts |> name
             val _ = if  tree is "UML:Model" then Logger.info ("parsing  model    "^package_name)
                     else                         Logger.info ("parsing  package  "^package_name)
         in
             XMI.Package 
                 { xmiid           = atts  |> xmiid, 
		   name            = atts  |> name,
		   visibility      = atts  |> visibility,
		   packages        = trees |> filterPackages    |> map mkPackage,
		   classifiers     = trees |> filterClassifiers |> map mkClassifier,
		   (*associations    = trees |> getAssociations,*) 
		   associations    = trees |> filterAssociations |> map mkAssociation,
		   generalizations = trees |> filter "UML:Generalization"
                                           |> map mkGeneralization,
		   constraints     = trees |> filterConstraints |> map mkConstraint,
		   stereotypes     = trees |> filterStereotypes |> map mkStereotype,
		   tag_definitions = trees |> filterTagDefinitions
                                           |> map mkTagDefinition,
		   state_machines  = nil,
		   activity_graphs = trees |> filterActivityGraphs
                                           |> map mkActivityGraph,
		   dependencies    = trees |> filterDependencies
                                           |> map mkDependency,
		   stereotype      = tree  |> get "UML:ModelElement.stereotype"
                                           |> map xmiidref, 
		   taggedValue     = tree  |> get "UML:ModelElement.taggedValue"
                                           |> map mkTaggedValue,
		   events          = trees |> filterEvents |> map mkEvent
                 }
	 end
    else Logger.error "no UML:Model or UML:Package found"
	 

fun mkXmiContent tree =
    let val trees = node_children (assert "XMI.content" tree)
    in
	{ packages              = trees |> filterPackages     |> map mkPackage,
	  constraints           = trees |> filterConstraints  |> map mkConstraint ,
	  classifiers           = trees |> filterClassifiers  |> map mkClassifier ,
	  stereotypes           = trees |> filterStereotypes  |> map mkStereotype ,
	  variable_declarations = trees |> filterVariableDecs |> map mkVariableDec ,
          activity_graphs       = trees |> filterActivityGraphs 
                                        |> map mkActivityGraph ,
          state_machines        = trees |> filterStateMachines 
                                        |> map mkStateMachine }
    end
    
val emptyXmiContent = { packages              = nil,
			constraints           = nil,
			classifiers           = nil,
			stereotypes           = nil,
			variable_declarations = nil,
                        activity_graphs       = nil,
                        state_machines        = nil}
                      
fun findXmiContent tree = valOf (dfs "XMI.content" tree)
    handle Option => Logger.error "no XMI.content found"
		     
fun readFile f = (mkXmiContent o findXmiContent o XmlTreeParser.readFile) f
    handle ex => (Logger.warn ("Error during parsing of "^f^": \n\t"^General.exnMessage ex); 
                  raise ex)
end


