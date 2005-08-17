(*****************************************************************************
 *          su4sml - a shallow embedding of OCL in Isabelle/HOL              
 *                                                                            
 * mds_parser.sig - signature for mds-parser for the import interface 
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>
 *                    J�rgen Doser <doserj@inf.ethz.ch>
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


signature  MDS_PARSER =
sig
    val list_classes : string ->  mdr_encoder.Classifier list
end
