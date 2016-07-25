# su4sml - a SML repository for managing (Secure)UML/OCL models

The model su4sml is a SML component thta stores UML models.  It follows the
UML/OCL metamodel in representing the model information as closely as this 
is sensible in a functional programming language. However, some 
simplifications where made deliberately. For example, we eliminated many 
indirections that are inherent in the UML metamodel. We also decided to 
ignore associations between classifiers as such. We only represent their 
association ends, as part of the participating classifiers

## Supported SML systems
* mlton 
* poly/ML 5.x or later
* sml/NJ

## License
This project is licensed under a 3-clause BSD-style license.

## Publications
* Achim D. Brucker, Jürgen Doser, and Burkhart Wolff. An MDA Framework Supporting 
  OCL. In Electronic Communications of the EASST, 5, 2006.
  (doi:10.14279/tuj.eceasst.5.45)[http://dx.doi.org/10.14279/tuj.eceasst.5.45]
