@// Simple template for Java 

@foreach nonprimitive_classifier_list
	@openfileifnotexists generated/src/main/java/$classifier_package_path$/$classifier_name$.java
	package $classifier_package$;
	@nl@nl
	import tudresden.ocl20.core.lib.*;
	@nl
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
	@// Operations of the class
	@foreach operation_list
		@// The actual Operation, which is called by the wrapper below
		@nl@tab/**@nl@tab
		@spc* Actual implementation of $operation_name$. @nl@tab
		@spc*/
		@nl@tabprivate $operation_result_type$ wrapped_$operation_name$(
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
		@// The wrapper which calls the actual operation above
		@nl@tab/**@nl@tab
		@spc* Wrapper to call $operation_name$ and check pre-/postconditions and invariants. @nl@tab
		@spc*/
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
			@spc{
			@if operation_non_void
				@nl@tab@tab$operation_result_type$ result;@nl
                        @end
			@nl@tab@tab// Check preconditions
			@nl$preconditions$
			@nl@tab@tab// Execute method @nl@tab@tab
			@if operation_non_void 
				result =@spc
			@end
			wrapped_$operation_name$(
			@foreach argument_list
			 	@if last_argument
					$argument_name$
				@else
					$argument_name$,@spc
				@end
			@end
			);
			@nl@nl@tab@tab// Check postconditions
			@nl$postconditions$
			@nl@tab@tab// Check invariant
			@nl@tab@tabcheckInvariant();
			@if operation_non_void
				@nl@tab@tabreturn result;
			@end
			@nl@tab}@nl
		@else
			;
		@end
	@end

	@nl@tab/**
	@nl@tab@spc* Invariant
	@nl@tab@spc*/
	@nl@tabpublic void checkInvariant() {
	@nl$invariants$
	@nl@tab}
	@nl}
@end 
