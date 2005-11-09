(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmi_umlcore.sig - XMI-UML-Core datatypes for the import interface for su4sml
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


(* ---------------------------------------------------------------------------
 * The types in these structures are supposed to define a representation of 
 * the XML elements found in UML-XMI files. It is reasonably close to the UML 
 * metamodel and the XMI representation of it, while simplifying some kinds 
 * of references.
 * --------------------------------------------------------------------------*)

structure XMI_OCL = 
struct
			   
(* FIX: LiteralExp should probably be renamed to PrimitiveLiteralExp *)
(* FIX: there should be also EnumLiteralExp and TupleLiteralExp *)
datatype OCLExpression = LiteralExp of { symbol          : string,
					 expression_type : string }
                       | CollectionLiteralExp of { parts: CollectionLiteralPart list,
						   expression_type : string}
		       | IfExp    of { condition       : OCLExpression,
				      thenExpression  : OCLExpression,
				      elseExpression  : OCLExpression,
				      expression_type : string }
       | AssociationEndCallExp   of { source                   : OCLExpression,
				       referredAssociationEnd   : string,
				       expression_type          : string }
       | AssociationClassCallExp of { source                   : OCLExpression, 
				       referredAssociationClass : string,
				       expression_type          : string }
       | AttributeCallExp        of { source            : OCLExpression,
				       referredAttribute : string,
				       expression_type   : string }
       | OperationCallExp        of { source            : OCLExpression,
				       arguments         : OCLExpression list,
				       referredOperation : string,
				       expression_type   : string }
       | OperationWithTypeArgExp of { source :OCLExpression,
				      name : string,
				      typeArgument: string,
				      expression_type: string} 
       | VariableExp             of { referredVariable: string,
				       expression_type : string }
       | LetExp                  of { variable        : VariableDeclaration,
				       inExpression    : OCLExpression,
				       expression_type : string }
       | IterateExp              of { iterators       : VariableDeclaration list, 
				       result          : VariableDeclaration ,
				       body            : OCLExpression,
				       source          : OCLExpression,
				       expression_type : string}
       | IteratorExp             of { name            : string, 
				       iterators       : VariableDeclaration list,
				       body            : OCLExpression,
				       source          : OCLExpression,
				       expression_type : string}
and CollectionLiteralPart = CollectionItem of { item : OCLExpression,
						expression_type: string }
                          | CollectionRange of { first: OCLExpression,
						 last: OCLExpression,
						 expression_type: string}
(* from OCL 2.0 Expressions: -------------------------------------------------
 * A VariableDeclaration declares a variable name and binds it to a type. The 
 * variable can be used in expressions where the variable is in scope. This 
 * metaclass represents amongst others the variables self and result and the 
 * variables defined using the Let expression.
 * not supported: initExpression 
 * --------------------------------------------------------------------------*)
withtype VariableDeclaration = { xmiid: string,
				 name: string,
				 declaration_type: string,
				 init: OCLExpression option}


fun expression_type_of (LiteralExp{expression_type,...})           = expression_type
  | expression_type_of (IfExp{expression_type,...})                = expression_type
  | expression_type_of (AssociationEndCallExp{expression_type,...}) = expression_type
  | expression_type_of (AssociationClassCallExp{expression_type,...}) = expression_type
  | expression_type_of (AttributeCallExp{expression_type,...}) = expression_type
  | expression_type_of (OperationCallExp{expression_type,...}) = expression_type
  | expression_type_of (OperationWithTypeArgExp{expression_type,...}) = expression_type 
  | expression_type_of (VariableExp{expression_type,...}) = expression_type
  | expression_type_of (LetExp{expression_type,...})           = expression_type
  | expression_type_of (IterateExp{expression_type,...})       = expression_type
  | expression_type_of (IteratorExp{expression_type,...})      = expression_type

(* from UML 1.5 Core: --------------------------------------------------------
 * A constraint is a semantic condition or restriction expressed in text.
 * not supported: 
 * --------------------------------------------------------------------------*)
datatype ConstraintType = Inv | Pre | Post | Def | Body

(* We put Constraint into OCL, not into XMI_Core because we only use *)
(* OCL Constraints.                                                  *)
type Constraint = { xmiid           : string,
		    name            : string option,
		    constraint_type : string, (* xmi.idref to stereotype *)
		    body            : OCLExpression }


end
 



