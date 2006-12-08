(** A Cartridge that contains a DESIGN_LANGUAGE. *)
signature DESIGN_LANGUAGE_CARTRIDGE =
sig	  
    
	include BASE_CARTRIDGE
	structure Design: DESIGN_LANGUAGE

end 
