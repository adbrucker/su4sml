.PHONY: doc

doc: 
	smldoc --directory=doc --linksource --overview=doc/overview.html src/su4sml.cm
