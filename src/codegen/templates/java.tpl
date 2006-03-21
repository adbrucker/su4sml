@// Example template for Java
@// assumption: all classifiers are classes

@foreach classifier_list
	@openfile generated/$classifier_name$.java
	package $classifier_package$ ;			
	@nl@nl
	public class $classifier_name$
	@if hasParent
		extends $classifier_parent$ 
	@end
	@nl {
	@foreach attribute_list
	@nl @tab	public $attribute_type$ $attribute_name$ ;
	@end
	@foreach operation_list
	@nl @tab  public $operation_result_type$ $operation_name$(
		@foreach argument_list
			$argument_type$ $argument_name$
		@end
		     )	
	@nl @tab  {} 
	@end
	@nl }
@end