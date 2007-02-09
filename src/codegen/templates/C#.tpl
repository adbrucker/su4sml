@// Template for C# 
@// (c) Copyright 2005 Raphael Eidenbenz eraphael-at-student.ethz.ch

@openfile generated/csharp/$classifier_package$.cs
// generated by su4sml GCG - Generic Code Generator 
@nl
@nl using System;
@nl
@nl namespace $classifier_package$ 
@nl {

@foreach classifier_list
	
	@nl@nl@nl
	@if isPrimitive
	@nl @tab // no support for primitive $classifier_name$ !!
	@nl
	@else
		@if isClass
		@nl @tab class $classifier_name$ 
		@elsif isInterface
		@nl @tab interface $classifier_name$
		@end
		@if hasParent
			: $classifier_parent$ 
		@end
		@nl @tab { 

		@foreach attribute_list

			@nl@nl
			@if attribute_isPublic @// PROPERTY!

				@nl @tab@tab private $attribute_scope$ $attribute_type$ $attribute_name_small_letter$ ;
				@nl @tab@tab public $attribute_scope$ $attribute_type$ $attribute_name_capital$
				@nl @tab@tab {
				@nl @tab@tab@tab get { return  $attribute_name_small_letter$; }
				@nl @tab@tab@tab set { $attribute_name_small_letter$ = value ; }
				@nl @tab@tab }
			@else
				@nl @tab@tab $attribute_visibility$ $attribute_scope$ $attribute_type$ $attribute_name$ ;
			@end

		@end
		@nl
		@foreach operation_list
		@nl @tab@tab  public $operation_scope$ $operation_result_type$ $operation_name$(
			@foreach argument_list
				@if last_argument 
					$argument_type$ $argument_name$
				@else 
					$argument_type$ $argument_name$ ,
				@end
			@end
			     )
		@nl @tab@tab {
		@nl @tab@tab@tab // ...toDo...
		@nl @tab@tab } 
		@end
		@// Konstruktor:
		@// @nl @tab@tab $classifier_name$() 
		@// @nl @tab@tab {}
		@nl @tab}
	
	@end

@end
@nl} // End