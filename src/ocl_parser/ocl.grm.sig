(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ocl.grm.sig --- 
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

signature OclParser_TOKENS =
sig
type ('a,'b) token
type svalue
val SIMPLE_NAME: (string) *  'a * 'a -> (svalue,'a) token
val STRING_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val REAL_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val INTEGER_LITERAL: (string) *  'a * 'a -> (svalue,'a) token
val NOT: (string) *  'a * 'a -> (svalue,'a) token
val LOG_IMPL: (string) *  'a * 'a -> (svalue,'a) token
val LOG_XOR: (string) *  'a * 'a -> (svalue,'a) token
val LOG_OR: (string) *  'a * 'a -> (svalue,'a) token
val LOG_AND: (string) *  'a * 'a -> (svalue,'a) token
val REL_LTE: (string) *  'a * 'a -> (svalue,'a) token
val REL_GTE: (string) *  'a * 'a -> (svalue,'a) token
val REL_LT: (string) *  'a * 'a -> (svalue,'a) token
val REL_GT: (string) *  'a * 'a -> (svalue,'a) token
val REL_NOTEQUAL: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val SLASH: (string) *  'a * 'a -> (svalue,'a) token
val STAR: (string) *  'a * 'a -> (svalue,'a) token
val MINUS: (string) *  'a * 'a -> (svalue,'a) token
val OCLASTYPE: (string) *  'a * 'a -> (svalue,'a) token
val OCLISKINDOF: (string) *  'a * 'a -> (svalue,'a) token
val OCLISTYPEOF: (string) *  'a * 'a -> (svalue,'a) token
val ISUNIQUE: (string) *  'a * 'a -> (svalue,'a) token
val ONE: (string) *  'a * 'a -> (svalue,'a) token
val EXISTS: (string) *  'a * 'a -> (svalue,'a) token
val ANY: (string) *  'a * 'a -> (svalue,'a) token
val COLLECT: (string) *  'a * 'a -> (svalue,'a) token
val REJECT: (string) *  'a * 'a -> (svalue,'a) token
val SELECT: (string) *  'a * 'a -> (svalue,'a) token
val ITERATE: (string) *  'a * 'a -> (svalue,'a) token
val GUARD: (string) *  'a * 'a -> (svalue,'a) token
val VERTICAL_BAR: (string) *  'a * 'a -> (svalue,'a) token
val BRACE_CLOSE: (string) *  'a * 'a -> (svalue,'a) token
val BRACE_OPEN: (string) *  'a * 'a -> (svalue,'a) token
val PAREN_CLOSE: (string) *  'a * 'a -> (svalue,'a) token
val PAREN_OPEN: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val POST: (Context.ConditionType) *  'a * 'a -> (svalue,'a) token
val PRE: (Context.ConditionType) *  'a * 'a -> (svalue,'a) token
val PACKAGE: (string) *  'a * 'a -> (svalue,'a) token
val LET: (string) *  'a * 'a -> (svalue,'a) token
val INV: (string) *  'a * 'a -> (svalue,'a) token
val INIT: (Context.AttrOrAssoc) *  'a * 'a -> (svalue,'a) token
val IN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val TRUE: (string) *  'a * 'a -> (svalue,'a) token
val FORALL: (string) *  'a * 'a -> (svalue,'a) token
val FALSE: (string) *  'a * 'a -> (svalue,'a) token
val ENDPACKAGE: (string) *  'a * 'a -> (svalue,'a) token
val ENDIF: (string) *  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val DERIVE: (Context.AttrOrAssoc) *  'a * 'a -> (svalue,'a) token
val DEF: (string) *  'a * 'a -> (svalue,'a) token
val CONTEXT: (string) *  'a * 'a -> (svalue,'a) token
val BODY: (Context.ConditionType) *  'a * 'a -> (svalue,'a) token
val DBL_CARAT: (string) *  'a * 'a -> (svalue,'a) token
val CARAT: (string) *  'a * 'a -> (svalue,'a) token
val BRACKET_CLOSE: (string) *  'a * 'a -> (svalue,'a) token
val BRACKET_OPEN: (string) *  'a * 'a -> (svalue,'a) token
val TUPLE_TYPE: (string) *  'a * 'a -> (svalue,'a) token
val TUPLE: (string) *  'a * 'a -> (svalue,'a) token
val SET: (string) *  'a * 'a -> (svalue,'a) token
val ORDERED_SET: (string) *  'a * 'a -> (svalue,'a) token
val SEQUENCE: (string) *  'a * 'a -> (svalue,'a) token
val COLLECTION: (string) *  'a * 'a -> (svalue,'a) token
val BAG: (string) *  'a * 'a -> (svalue,'a) token
val AT_PRE: (string) *  'a * 'a -> (svalue,'a) token
val HASH: (string) *  'a * 'a -> (svalue,'a) token
val QUESTION_MARK: (string) *  'a * 'a -> (svalue,'a) token
val EQUALS: (string) *  'a * 'a -> (svalue,'a) token
val SEMI_COLON: (string) *  'a * 'a -> (svalue,'a) token
val DBL_COLON: (string) *  'a * 'a -> (svalue,'a) token
val COLON: (string) *  'a * 'a -> (svalue,'a) token
val DBL_DOT: (string) *  'a * 'a -> (svalue,'a) token
val DOT: (string) *  'a * 'a -> (svalue,'a) token
val ARROW_RIGHT: (string) *  'a * 'a -> (svalue,'a) token
val COMMA: (string) *  'a * 'a -> (svalue,'a) token
val TICK: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature OclParser_LRVALS=
sig
structure Tokens : OclParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
