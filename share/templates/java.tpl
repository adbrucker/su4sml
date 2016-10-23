@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- an SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// java.tpl --- simple template for Java 
@// This file is part of su4sml.
@//
@// Copyright (c) 2005-2007, ETH Zurich, Switzerland
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
@// $Id$


@foreach nonprimitive_classifier_list
	@openfileifnotexists generated/src/main/java/$classifier_package_path$/$classifier_name$.java
	package $classifier_package$;
	@nl@nl
	@if isClass
		public class $classifier_name$
	@end
	@if isInterface
		public interface $classifier_name$
	@end
	@if isEnumeration
		public enum $classifier_name$
	@end
	@if notInterface
		@if hasParent
			@spcextends $classifier_parent$ 
		@end
	@end
	@if hasParentInterfaces
		@if isInterface
			@spcextends
		@else
			@spcimplements 
		@end
		@foreach parent_interface_list
			@if last_interface
				$parent_interface$
			@else
				$parent_interface$,
			@end
		@end
	@end
	@spc{
	@if notInterface
		@foreach attribute_list
			 @nl@tab$attribute_visibility$ $attribute_type$ $attribute_name$;
		@end
		@nl
	@end
	@foreach operation_list
		@nl@tab$operation_visibility$ $operation_result_type$ $operation_name$(
		@foreach argument_list
			@if last_argument
				$argument_type$ $argument_name$
			@else
				$argument_type$ $argument_name$,@spc
			@end
		@end
		)
		@if notInterface	
	    	    	@spc{@nl
			@if operation_non_void
				@tab@tabreturn $returnvalue_stub$;
			@end
			@nl@tab}@nl
		@else
			;
		@end
	@end
	@nl}
@end 
