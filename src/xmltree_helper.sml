(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmltree_helper.sml - helper functions for xml trees
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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


structure XmlTreeHelper : sig
    (*     val skip        : string -> Tree -> Tree list  *)
    val get_many    : string -> XmlTree.Tree -> XmlTree.Tree list
    val get_one     : string -> XmlTree.Tree -> XmlTree.Tree 
    val get_maybe   : string -> XmlTree.Tree -> XmlTree.Tree list
    val get         : string -> XmlTree.Tree -> XmlTree.Tree list 
    val get_optional: string -> XmlTree.Tree -> XmlTree.Tree option         
    val filter      : string -> XmlTree.Tree list -> XmlTree.Tree list
    val filter_children : string -> XmlTree.Tree -> XmlTree.Tree list
    val find_some   : string -> XmlTree.Tree list -> XmlTree.Tree option
    val find        : string -> XmlTree.Tree list -> XmlTree.Tree
    val find_child  : string -> XmlTree.Tree -> XmlTree.Tree
    val dfs         : string -> XmlTree.Tree -> XmlTree.Tree option
    val exists      : string -> XmlTree.Tree list -> bool
    val has_child   : string -> XmlTree.Tree -> bool
    val value_of    : string -> XmlTree.Attribute list -> string 
    (*     val follow      : string -> XmlTree.Tree list -> XmlTree.Tree list *)
    (*     val followM     : string -> XmlTree.Tree list -> XmlTree.Tree list  *)
    (* val skipM       : string -> XmlTree.Tree -> XmlTree.Tree list *)
    val assert      : string -> XmlTree.Tree -> XmlTree.Tree
    val is          : XmlTree.Tree * string -> bool
    (*    val follow_all  : string -> XmlTree.Tree list -> XmlTree.Tree list list *)
                                                 
    (*    val apply_on    : string -> (Attribute list -> 'a) -> XmlTree.Tree -> 'a*)
    val some_id : XmlTree.Tree -> string
    val some_id': XmlTree.Attribute list -> string 
end =
struct 
open library
open XmlTree
infix 1 |>
fun filter string trees = List.filter (fn x => string = tagname x) 
				      trees

fun filter_children string tree = filter string (node_children tree)
				      
fun find_some string trees = (List.find (fn x => string = tagname x) trees)

fun find string trees = valOf (List.find (fn x => string = tagname x) trees) 
    handle Option => raise IllFormed ("in XmlTree.find: no element "
                                      ^string)


fun some_id' atts = let val xmiid = atts |> optional_value_of "xmi.id" 
                        val xmiidref = atts |> optional_value_of "xmiidref" 
                    in 
                        if Option.isSome xmiid then 
                            " (xmi.id = "^
                            (Option.valOf xmiid)^
                            ")"
                        else if Option.isSome xmiidref then 
                            " (xmi.idref = "^
                            (Option.valOf xmiidref)^
                            ")"
                        else ""
                    end

fun some_id tree = some_id' (attributes tree)

fun value_of string atts = XmlTree.value_of string atts
    handle IllFormed msg => raise IllFormed (msg^(some_id' atts))
 
fun find_child string tree = find string (node_children tree)
    handle IllFormed msg => raise IllFormed (msg^" inside node "^
                                             (tagname tree)^(some_id tree)^"\n")
			   
fun dfs string tree = if tagname tree = string 
		      then SOME tree
		      else Option.join (List.find Option.isSome (List.map (dfs string) (node_children tree)))

fun exists string trees = List.exists (fn x => string = tagname x) trees 
fun has_child string tree = exists string (node_children tree) 
			  
fun follow  string trees = node_children (find string trees)
fun skip    string tree  = node_children (find_child string tree)
fun followM string trees  = follow string trees   handle IllFormed msg => nil
fun skipM   string tree   = skip string tree      handle IllFormed msg => nil

fun is (tree,string) = string = tagname tree
infix 2 is
fun assert string tree  = if tree is string then tree
                          else raise IllFormed ("expected "^string^" but found "^
                                                (tagname tree)^(some_id tree)^"\n")

(* navigate to association ends with multiplicity 1..* *)
fun get_many string tree = skip string tree
(* navigate to association ends with multiplicity 1 *)
fun get_one string tree = hd (skip string tree)
(* navigate to association ends with multiplicity 0..* *)
fun get_maybe string tree = skipM string tree 
val get = get_maybe
(* navigate to association ends with multiplicity 0..1 *)
fun get_optional string tree = Option.map (hd o node_children)
                                          (find_some string (node_children tree))

(* fun follow_all string trees = map node_children (filter string trees) *)
			     			    
end
