@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- an SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// use.tpl --- Template for USE (UML Specification Environment)
@// This file is part of su4sml.
@//
@// Copyright (c) 2007, ETH Zurich, Switzerland
@//
@// All rights reserved.
@//
@// Redistribution and use in source and binary forms, with or without
@// modification, are permitted provided that the following conditions are
@// met:
@//
@//     * Redistributions of source code must retain the above copyright
@//       notice, this list of conditions and the following disclaimer.
@//
@//     * Redistributions in binary form must reproduce the above
@//       copyright notice, this list of conditions and the following
@//       disclaimer in the documentation and/or other materials provided
@//       with the distribution.
@//
@//     * Neither the name of the copyright holders nor the names of its
@//       contributors may be used to endorse or promote products derived
@//       from this software without specific prior written permission.
@//
@// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
@// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
@// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
@// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
@// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
@// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
@// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
@// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
@// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
@// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
@// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@///////////////////////////////////////////////////////////////////////////////
@// $Id: use.tpl 40127 2007-07-04 06:41:30Z brucker $




@openfile $classifier_package$.use

@nl
@nl model $classifier_package$

@foreach nonprimitive_classifier_list
	@nl
	@nl
	@if isClass
		class $classifier_name$
	@end
	@if isInterface
		interface $classifier_name$
	@end
	@if isEnumeration
		enum $classifier_name$ {
	@end
	@if notInterface
		@if hasParent
			< $classifier_parent$ 
		@end
	@end
	@nl
	@if notInterface
	    @if hasAttributes	
		@spc@spc@spc@spc attributes
		@foreach attribute_list
			 @nl@tab$attribute_name$ : $attribute_type$ 
		@end
		@nl
            @end
	@end
	@nl 
	@if hasOperations
	   @spc@spc@spc@spc operations
		@foreach operation_list
	              @nl @tab@tab  $operation_name$(
                          @foreach argument_list
                                @if last_argument
                                        $argument_name$ : $argument_type$
                                @else
                                       $argument_name$ : $argument_type$,
                                @end
                      @end
                      ) : $operation_result_type$ 
	        @end
	    @end
	@nl 
	@if isClass
	    end
	@end
	@if isEnumeration
		}
	@end


@end 
@nl
@nl

constraints@nl
@foreach nonprimitive_classifier_list
  @if hasInvariants
    context $classifier_name$ @nl
	@foreach invariant_list 
          @spc@spc inv $inv_name$: $inv_cs$ @nl
        @end
  @nl
  @end

  @if hasOperations
    @foreach operation_list
      @if hasOpSpec
	 @nl context $classifier_name$::$operation_name$(
         @foreach argument_list
           @if last_argument
             $argument_name$ : $argument_type$
           @else
             $argument_name$ : $argument_type$,
           @end
         @end
         ) : $operation_result_type$ 
      	 @nl 
	 @foreach precondition_list 
           @spc@spc pre $pre_name$: $pre_cs$ @nl
         @end
      	 @foreach postcondition_list 
           @spc@spc post $post_name$: $post_cs$ @nl
         @end
      @end
      @nl
    @end
  @end
 
@end

@end

