(** very simple structure defining basix xml schema types. *)
structure XML_Schema = 
struct 
(* for simplicity: *)

(** 
 * XML namespace NCName. 
 * the basic grammar is: 
 * NCName ::= (Letter | '_') (Letter | Digit | '.' | '-' | '_')* 
 *)
type NCName = string

(** 
 * XML namespace QName, i.e., a NCName with optionally a namespace prefix. 
 * the basic grammar is: QName ::= (NCName ':')? NCName    
 *)
type QName  = string 

type anyURI = string (* a URI like "http://www.example.com/file.html#id5"*)


type xsdElement = { name  : string,
                    typ   : string }

type xsdSequence = xsdElement list


datatype xsdType = SimpleType of xsdElement
                 | ComplexType of xsdComplexType
withtype xsdComplexType = { name : string,
                            sequence : xsdType list
                          }

type xsdSchema = { attributeFormDefault: string,
                   elementFormDefault:   string,
                   targetNamespace:      string,
                   definitions:          xsdType list
                 }

end


structure XML_Schema2Xml =
struct
open XML_Schema


fun xsdType2Xml (ComplexType ct) = 
    XmlTree.Node (("xsd:complexType",[("name", #name ct)
                  ]),
                  [
                   XmlTree.Node (("xsd:sequence",[]),
                                 map xsdType2Xml (#sequence ct)
                                )
                  ]
                 )
  | xsdType2Xml (SimpleType st) = 
    XmlTree.Node (("xsd:element",[("name",#name st),("type",#typ st)]),[])

fun xsdSchema2Xml (schema:xsdSchema) = 
    XmlTree.Node (("xsd:schema",[("attributeFormDefault",#attributeFormDefault schema),
			         ("elementFormDefault", #elementFormDefault schema),
			         ("targetNamespace", #targetNamespace schema)
                  ]),
                  map xsdType2Xml (#definitions schema))



end
