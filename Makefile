.PHONY: doc apidoc tags

doc: 
	smldoc --directory=doc --linksource --showsummary --overview=doc/overview.html --windowtitle="SU4SML_Documentation" src/su4sml.cm

apidoc: 
	smldoc --windowtitle="SU4SML API Documentation" --hidebysig --directory=doc/api --linksource --showsummary --overview=doc/overview.html src/su4sml.cm

codegen-doc:
	smldoc --windowtitle="GCG Documentation" --directory=codegen-doc --linksource --showsummary --overview=doc/overview.html --linkoffline=../doc@doc/module-list src/codegen/codegen.cm

tags:
	cd src && etags --language=none --regex='/^\(datatype\|type\|val\|fun\|structure\|signature\)[ \t]*\([^ \t]*\)/\2/' *.sml *.sig codegen/*.sml codegen/*.sig
su4sml:
	(cd src && echo "val _ = use \"ROOT.ML\"; val _ = OS.Process.exit 0" | sml ) || true

rep_parser: 
	ml-build src/su4sml.cm RepParser.test src/rep_parser

repsecure_parser:
	ml-build src/su4sml.cm Rep_SecureUML_ComponentUML.test src/repsecure_parser

codegen: 
	ml-build src/codegen/codegen.cm Codegen.main codegen

