# su4sml: An SML repository for managing SecureUML/OCL and UML/OCL models

The model repository *su4sml* is an SML component that stores UML
models.  It follows the UML/OCL metamodel in representing the model
information as closely as this is sensible in a functional programming
language. However, some simplifications where made deliberately. For
example, we eliminated many indirections that are inherent in the UML
metamodel. We also decided to ignore associations between classifiers
as such. We only represent their association ends, as part of the
participating classifiers

## Supported SML systems
* mlton 
* poly/ML 5.x or later
* sml/NJ

## Developers 
* [Achim D. Brucker](http://www.brucker.ch/)
* Jürgen Doser
* Burkhart Wolff

### Contributors
* Rolf Adelsberger
* Martin Bill
* Raphael Eidenbenz
* Manuel Krucker
* Raphael Schmid
* Manfred Stock

## License
This project is licensed under a 3-clause BSD (SPDIX: BSD-3-Clause)  license.

## Publications
* Achim D. Brucker, Jürgen Doser, and Burkhart Wolff. An MDA Framework Supporting 
  OCL. In Electronic Communications of the EASST, 5, 2006.
  [doi:10.14279/tuj.eceasst.5.45](http://dx.doi.org/10.14279/tuj.eceasst.5.45)
