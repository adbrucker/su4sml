(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * context_declarations.sml --- 
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
(* $Id: context_declarations.sml 6727 2007-07-30 08:43:40Z brucker $ *)
signature WFCPO_SYNTAX_ERROR_DB =
sig
    datatype SyntaxError =
	     SEType of  
	     { fromClass : Rep_Core.Classifier,
	       toClass   : Rep_Core.Classifier,
	       fromOp    : Rep_Core.operation,
               toOp      : Rep_Core.operation }
	   | SEOp of
	     { fromClass : Rep_Core.Classifier,
	       ops       : Rep_Core.operation list }
	   | SEClass of 
	     { package   : Rep_OclType.Path,
	       classes   : Rep_Core.Classifier list}
	     
    val SE_db            : ((SyntaxError option) list) ref 
    val add_syntax_error : SyntaxError -> unit
end;
structure WFCPO_Syntax_Error_DB:WFCPO_SYNTAX_ERROR_DB = 
struct 

open Rep_Core
open Rep_OclType

datatype SyntaxError =
	 SEType of  
	 { fromClass : Classifier,
	   toClass   : Classifier,
	   fromOp    : operation,
           toOp      : operation }
       | SEOp of
	 { fromClass : Classifier,
	   ops       : operation list }
       | SEClass of 
	 { package   : Path,
	   classes   : Classifier list }
		    
val SE_db:SyntaxError option list ref = ref [NONE]

fun add_syntax_error (s:SyntaxError) = 
    let 
	val x = (SOME(s)::(!SE_db))
    in
	SE_db := x
    end

end;
