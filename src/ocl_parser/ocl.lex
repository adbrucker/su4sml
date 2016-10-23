(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ocl.lex --- 
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


open Rep_OclTerm
open Rep_OclType
open Context

structure Tokens = Tokens

type pos = int * int * int
type svalue = Tokens.svalue

type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token


val pos = ref (0,0,0)

  fun eof () = Tokens.EOF((!pos,!pos))
  fun error (e,p : (int * int * int),_) = TextIO.output (TextIO.stdOut, 
							 String.concat[
								       "line ", (Int.toString (#1 p)), "/",
								       (Int.toString (#2 p - #3 p)),": ", e, "\n"
								       ])
  
 fun inputPos yypos = ((#1 (!pos), yypos - (#3(!pos)), (#3 (!pos))),
		     (#1 (!pos), yypos - (#3(!pos)), (#3 (!pos)))) 
 fun inputPos_half yypos = (#1 (!pos), yypos - (#3(!pos)), (#3 (!pos)))



%%
%header (functor OclParserLexFun(structure Tokens: OclParser_TOKENS));
alpha=[A-Za-z_];
digit=[0-9];
ws = [\ \t];
%%

\n       => (pos := ((#1 (!pos)) + 1, yypos - (#3(!pos)),yypos  ); lex());
{ws}+    => (pos := (#1 (!pos), yypos - (#3(!pos)), (#3 (!pos))); lex()); 

(--)[^\n]*\n                    => (pos := ((#1 (!pos)) + 1, yypos - (#3(!pos)),yypos  ); lex());

"/*""/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/" => (lex());


","        => (Tokens.COMMA(yytext,inputPos_half yypos,inputPos_half yypos));
"->"       => (Tokens.ARROW_RIGHT(yytext,inputPos_half yypos,inputPos_half yypos));
"."        => (Tokens.DOT(yytext,inputPos_half yypos,inputPos_half yypos));
".."       => (Tokens.DBL_DOT(yytext,inputPos_half yypos,inputPos_half yypos));
":"        => (Tokens.COLON(yytext,inputPos_half yypos,inputPos_half yypos));
"::"       => (Tokens.DBL_COLON(yytext,inputPos_half yypos,inputPos_half yypos));
";"        => (Tokens.SEMI_COLON(yytext,inputPos_half yypos,inputPos_half yypos));
"="	   => (Tokens.EQUALS(yytext,inputPos_half yypos,inputPos_half yypos));
"?"        => (Tokens.QUESTION_MARK(yytext,inputPos_half yypos,inputPos_half yypos));
"#"        => (Tokens.HASH(yytext,inputPos_half yypos,inputPos_half yypos));
"@pre"     => (Tokens.AT_PRE(yytext,inputPos_half yypos,inputPos_half yypos));
"Bag"      => (Tokens.BAG(yytext,inputPos_half yypos,inputPos_half yypos));
"Collection" => (Tokens.COLLECTION(yytext,inputPos_half yypos,inputPos_half yypos));
"OrderedSet" => (Tokens.ORDERED_SET(yytext,inputPos_half yypos,inputPos_half yypos));
"Sequence" => (Tokens.SEQUENCE(yytext,inputPos_half yypos,inputPos_half yypos));
"Set"      => (Tokens.SET(yytext,inputPos_half yypos,inputPos_half yypos));
"Tuple"    => (Tokens.TUPLE(yytext,inputPos_half yypos,inputPos_half yypos));
"TupleType"  => (Tokens.TUPLE_TYPE(yytext,inputPos_half yypos,inputPos_half yypos));
"["        => (Tokens.BRACKET_OPEN(yytext,inputPos_half yypos,inputPos_half yypos));
"]"        => (Tokens.BRACKET_CLOSE(yytext,inputPos_half yypos,inputPos_half yypos));
"^"        => (Tokens.CARAT(yytext,inputPos_half yypos,inputPos_half yypos));
"^^"       => (Tokens.DBL_CARAT(yytext,inputPos_half yypos,inputPos_half yypos));
"body"     => (Tokens.BODY(body,inputPos_half yypos,inputPos_half yypos));
"context"  => (Tokens.CONTEXT(yytext,inputPos_half yypos,inputPos_half yypos));
"def"      => (Tokens.DEF(yytext,inputPos_half yypos,inputPos_half yypos));
"derive"   => (Tokens.DERIVE(derive,inputPos_half yypos,inputPos_half yypos));
"else"     => (Tokens.ELSE(yytext,inputPos_half yypos,inputPos_half yypos));
"endif"    => (Tokens.ENDIF(yytext,inputPos_half yypos,inputPos_half yypos));
"endpackage" => (Tokens.ENDPACKAGE(yytext,inputPos_half yypos,inputPos_half yypos));
"false"    => (Tokens.FALSE(yytext,inputPos_half yypos,inputPos_half yypos));
"null"     => (Tokens.NULL(yytext,inputPos_half yypos,inputPos_half yypos));
"if"       => (Tokens.IF(yytext,inputPos_half yypos,inputPos_half yypos));
"in"       => (Tokens.IN(yytext,inputPos_half yypos,inputPos_half yypos));
"init"     => (Tokens.INIT(init,inputPos_half yypos,inputPos_half yypos));
"inv"      => (Tokens.INV(yytext,inputPos_half yypos,inputPos_half yypos));
"let"      => (Tokens.LET(yytext,inputPos_half yypos,inputPos_half yypos));
"package"  => (Tokens.PACKAGE(yytext,inputPos_half yypos,inputPos_half yypos));
"pre"      => (Tokens.PRE(pre,inputPos_half yypos,inputPos_half yypos));
"post"     => (Tokens.POST(post,inputPos_half yypos,inputPos_half yypos));
"then"     => (Tokens.THEN(yytext,inputPos_half yypos,inputPos_half yypos));
"true"     => (Tokens.TRUE(yytext,inputPos_half yypos,inputPos_half yypos));
"("        => (Tokens.PAREN_OPEN(yytext,inputPos_half yypos,inputPos_half yypos));
")"        => (Tokens.PAREN_CLOSE(yytext,inputPos_half yypos,inputPos_half yypos));
"{"        => (Tokens.BRACE_OPEN(yytext,inputPos_half yypos,inputPos_half yypos));
"}"        => (Tokens.BRACE_CLOSE(yytext,inputPos_half yypos,inputPos_half yypos));
"|"        => (Tokens.VERTICAL_BAR(yytext,inputPos_half yypos,inputPos_half yypos));
"guard"    => (Tokens.GUARD(yytext,inputPos_half yypos,inputPos_half yypos));

"iterate"  => (Tokens.ITERATE(yytext,inputPos_half yypos,inputPos_half yypos));
"select"   => (Tokens.SELECT(yytext,inputPos_half yypos,inputPos_half yypos));
"reject"   => (Tokens.REJECT(yytext,inputPos_half yypos,inputPos_half yypos));
"collect"  => (Tokens.COLLECT(yytext,inputPos_half yypos,inputPos_half yypos));
"forAll"   => (Tokens.FORALL(yytext,inputPos_half yypos,inputPos_half yypos));
"any"      => (Tokens.ANY(yytext,inputPos_half yypos,inputPos_half yypos));
"exists"   => (Tokens.EXISTS(yytext,inputPos_half yypos,inputPos_half yypos));
"one"      => (Tokens.ONE(yytext,inputPos_half yypos,inputPos_half yypos));
"isUnique" => (Tokens.ISUNIQUE(yytext,inputPos_half yypos,inputPos_half yypos));

"oclIsTypeOf" => (Tokens.OCLISTYPEOF(yytext,inputPos_half yypos,inputPos_half yypos));
"oclIsKindOf" => (Tokens.OCLISKINDOF(yytext,inputPos_half yypos,inputPos_half yypos));
"oclAsType"   => (Tokens.OCLASTYPE(yytext,inputPos_half yypos,inputPos_half yypos));
"-"	   => (Tokens.MINUS(yytext,inputPos_half yypos,inputPos_half yypos));
"*"	   => (Tokens.STAR(yytext,inputPos_half yypos,inputPos_half yypos));
"/"        => (Tokens.SLASH(yytext,inputPos_half yypos,inputPos_half yypos));
"+"	   => (Tokens.PLUS(yytext,inputPos_half yypos,inputPos_half yypos));

">"  	   => (Tokens.REL_GT(yytext,inputPos_half yypos,inputPos_half yypos));
"<"	   => (Tokens.REL_LT(yytext,inputPos_half yypos,inputPos_half yypos));
">="	   => (Tokens.REL_GTE(yytext,inputPos_half yypos,inputPos_half yypos));
"<="	   => (Tokens.REL_LTE(yytext,inputPos_half yypos,inputPos_half yypos));
"<>"       => (Tokens.REL_NOTEQUAL(yytext,inputPos_half yypos,inputPos_half yypos));

"and"	   => (Tokens.LOG_AND(yytext,inputPos_half yypos,inputPos_half yypos));
"or"	   => (Tokens.LOG_OR(yytext,inputPos_half yypos,inputPos_half yypos));
"xor"	   => (Tokens.LOG_XOR(yytext,inputPos_half yypos,inputPos_half yypos));
"implies"  => (Tokens.LOG_IMPL(yytext,inputPos_half yypos,inputPos_half yypos));

"not"      => (Tokens.NOT(yytext,inputPos_half yypos,inputPos_half yypos));

"model"    => (Tokens.MODEL(yytext,inputPos_half yypos,inputPos_half yypos));
"class"    => (Tokens.CLASS(yytext,inputPos_half yypos,inputPos_half yypos));
"attributes" => (Tokens.ATTRIBUTES(yytext,inputPos_half yypos,inputPos_half yypos));
"operations" => (Tokens.OPERATIONS(yytext,inputPos_half yypos,inputPos_half yypos));
"constraints" => (Tokens.CONSTRAINTS(yytext,inputPos_half yypos,inputPos_half yypos)); 
"association" => (Tokens.ASSOCIATIONS(yytext,inputPos_half yypos,inputPos_half yypos));
"between"     => (Tokens.BETWEEN(yytext,inputPos_half yypos,inputPos_half yypos));
"end"         => (Tokens.END(yytext,inputPos_half yypos,inputPos_half yypos));


{digit}+"."{digit}+(E|e)?({digit})*   => (Tokens.REAL_LITERAL(yytext,inputPos_half yypos,inputPos_half yypos));
{digit}+                          => (Tokens.INTEGER_LITERAL(yytext,inputPos_half yypos,inputPos_half yypos));
"'"({alpha}|{ws}|{digit})*(("."|"_"|"/"|"-")*({alpha}|{ws}|{digit})*)*"'"  => (Tokens.STRING_LITERAL(yytext,inputPos_half yypos,inputPos_half yypos));
{alpha}+(_)?({alpha}|{digit})*      => (Tokens.SIMPLE_NAME(yytext,inputPos_half yypos,inputPos_half yypos));




.      => (error ("ignoring bad character "^yytext,
		    ((#1 (!pos), yypos - (#3(!pos)), (#3 (!pos)))),
		    ((#1 (!pos), yypos - (#3(!pos)), (#3 (!pos)))));
             lex());
