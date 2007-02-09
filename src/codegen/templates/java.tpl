@// Example template for Java
@// assumption: all classifiers are classes

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
			@tab extends $classifier_parent$ 
		@end
	@end
	@if hasParentInterfaces
		@if isInterface
			@tab extends
		@else
			@tab implements 
		@end
		@foreach parent_interface_list
			@if last_interface
				$parent_interface$
			@else
				$parent_interface$,
			@end
		@end
	@end
	{
	@if notInterface
		@foreach attribute_list
			 @nl @tab $attribute_visibility$ $attribute_type$ $attribute_name$ ;
		@end
	@end
	@foreach operation_list
		@nl @tab  $operation_visibility$ $operation_result_type$ $operation_name$(
		@foreach argument_list
			@if last_argument
				$argument_type$ $argument_name$
			@else
				$argument_type$ $argument_name$,
			@end
		@end
		)
		@if notInterface	
	    	    	{@nl@nl@tab} 
		@else
			;
		@end
	@end
	@nl }
@end 
