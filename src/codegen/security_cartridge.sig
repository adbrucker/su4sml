signature SECURITY_LANGUAGE_CARTRIDGE =
sig	  
    structure Security: SECURITY_LANGUAGE
							
    include BASE_CARTRIDGE where 
	type Model = Rep.Classifier list * Security.Configuration

end 
