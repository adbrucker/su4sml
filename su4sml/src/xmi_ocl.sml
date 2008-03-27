
(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi_ocl.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
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
datatype OCLExpression = LiteralExp              of { symbol          : string,
					              expression_type : string }
                       | CollectionLiteralExp    of { parts: CollectionLiteralPart list,
						      expression_type : string}
		       | IfExp                   of { condition       : OCLExpression,
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
     and CollectionLiteralPart = CollectionItem  of { item : OCLExpression,
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
  | expression_type_of (CollectionLiteralExp{expression_type,...}) = expression_type
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

fun expression_source_of (AssociationEndCallExp{source,...}) = source
  | expression_source_of (AssociationClassCallExp{source,...}) = source
  | expression_source_of (AttributeCallExp{source,...}) = source
  | expression_source_of (OperationCallExp{source,...}) = source
  | expression_source_of (OperationWithTypeArgExp{source,...}) = source 
  | expression_source_of (IterateExp{source,...})       = source
  | expression_source_of (IteratorExp{source,...})      = source
  | expression_source_of _                              = Rep_Logger.error ("expression has no source")

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
 



