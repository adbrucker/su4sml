(* a design language specifies what the protected resources and the *)
(* possible actions on these resources are                          *)
signature DESIGN_LANGUAGE_CARTRIDGE =
sig	  
    
	include CARTRIDGE
	structure Design: DESIGN_LANGUAGE


end 
