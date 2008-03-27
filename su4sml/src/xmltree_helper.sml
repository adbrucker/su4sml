(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmltree_helper.sml --- helper functions for xml trees
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)


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
(*  val follow      : string -> XmlTree.Tree list -> XmlTree.Tree list *)
(*  val followM     : string -> XmlTree.Tree list -> XmlTree.Tree list  *)
(*  val skipM       : string -> XmlTree.Tree -> XmlTree.Tree list *)
    val assert      : string -> XmlTree.Tree -> XmlTree.Tree
    val is          : XmlTree.Tree * string -> bool
(*  val follow_all  : string -> XmlTree.Tree list -> XmlTree.Tree list list *)
(*  val apply_on    : string -> (Attribute list -> 'a) -> XmlTree.Tree -> 'a*)
    val some_id : XmlTree.Tree -> string
    val some_id': XmlTree.Attribute list -> string 
end =
struct
open Rep_Help_Functions 
open Rep_Logger
open XmlTree

infix 1 |>

fun filter string trees = List.filter (fn x => string = tagname x) trees

fun filter_children string tree = filter string (node_children tree)
				      
fun find_some string trees = (List.find (fn x => string = tagname x) trees)

fun find string trees = valOf (List.find (fn x => string = tagname x) trees) 
    handle Option => error ("in XmlTree.find: no element "^string)


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
    handle ex => error ((General.exnMessage ex)^(some_id' atts))
 
fun find_child string tree = find string (node_children tree)
    handle ex => error ((General.exnMessage ex)^" inside node "^(tagname tree)^(some_id tree)^"\n")
			   
fun dfs string tree = if tagname tree = string 
		      then SOME tree
		      else Option.join (List.find Option.isSome (List.map (dfs string) (node_children tree))) 

fun exists string trees = List.exists (fn x => string = tagname x) trees 
fun has_child string tree = exists string (node_children tree) 
			  
fun follow  string trees = node_children (find string trees)
fun followM string trees  = if   exists string trees
                            then follow string trees  
                            else nil

fun skip    string tree  = node_children (find_child string tree)
fun skipM   string tree   = if  has_child string tree 
                            then skip string tree     
                            else nil
                            

fun is (tree,string) = string = tagname tree
infix 2 is
fun assert string tree  = if tree is string then tree
                          else error ("expected "^string^" but found "^
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
