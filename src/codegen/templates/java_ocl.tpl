@// Example template for Java
@// assumption: all classifiers are classes

@foreach classifier_list
	@openfile generated/$classifier_package_path$/$classifier_name$.java
	package $classifier_package$ ;			
	@nl@nl
	@if isClass
		public class $classifier_name$
	@end
	@if isInterface
		public interface $classifier_name$
	@end
	@if notInterface
		@if hasParent
			extends $classifier_parent$ 
		@end
	@end
	@if hasParentInterfaces
		@if isInterface
			extends
		@else
			implements 
		@end
		@foreach parent_interface_list
			@if last_interface
				$parent_interface$
			@else
				$parent_interface$,
			@end
		@end
	@end
	@nl {
	@if notInterface
		@foreach attribute_list
			 @nl @tab	public $attribute_type$ $attribute_name$ ;
		@end
	@end
	@foreach operation_list
	@nl @tab  public $operation_result_type$ $operation_name$(
		@foreach argument_list
			$argument_type$ $argument_name$
		@end
		     )
	@if notInterface 
	    	{
	    	@nl @tab @tab // Preconditions
	    	@nl $preconditions$
		@nl @tab @tab // Your Code
	    	@nl @tab @tab // Postconditions
	    	@nl $postconditions$
		@nl@tab } 
	@else
		;
	@end
	@end
	@nl // Invariant 
	@nl $invariants$
	@nl
	@nl }
@end 
