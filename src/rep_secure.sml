(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * rep_secure.sml - repository structure for uml models with security 
 *                  specifications
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)


functor Rep_Secure(structure Security : SECURITY_LANGUAGE) : REP_SECURE =
struct
structure Security = Security

type Model = Rep_Core.Classifier list * Security.Configuration
	     

val readXMI = Security.parse o Xmi2Rep.readXMI
end


structure Rep_SecureUML_ComponentUML 
  = Rep_Secure(structure Security = SecureUML(structure Design=ComponentUML))
			
