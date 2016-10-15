# uml2cdl - A converter from uml activity diagrams to WS-CDL


 * `src/` contains the main sml-sources for the converter. If you have
   mlton installed, try typing 'make uml2cdl' or 'make uml2cdl-cygwin'
   in the src/ directory.  If you are using SML/NJ, try typing
   'CM.make "uml2cdl.cm"' in you sml environment.  If you are using
   some other ML system, try typing 'use "ROOT.ML"'

 * `lib/` contains libraries that are needed for building uml2cdl.  One
   dependency is 'su4sml', an sml library for working with .xmi
   files that is being developed internally in the contexts of work on
   Model-Driven Security and Isabelle/HOL-OCL.

   The other dependency is 'fxp', a functional xml parser written in
   sml, on top of which su4sml implements parsing of .xmi files. 

 * `doc/` contains preliminary documentation for uml2cdl.  Build it by
   typing 'latex cdl-profile.tex'

 * `examples/` contains some example files for testing the functionality
   of uml2cdl.

 * `contrib/` contains additional files that may be useful for actually
   using uml2cdl.  Most notably it contains 'Uml2CdlClient.java',
   which shows how one can use the uml2cdl functionality from
   Java-code by using (local or remote) socket communication.  This
   assumes that 'inetd' is running on the respective machine and has
   been configured to execute 'uml2cdl' on connects to the specified
   port. 
