# jtestdataaccessor
This add-on to su4sml provides access to test data in unit tests.

## Installation
Prerequisites:
 * Java 1.5
 * [Apache Maven 2](http://maven.apache.org/)  or AntLR >= 2.7.6 and JUnit >= 4.0

After installing the prerequisites, a simple call to 
```
$ mvn install
```
in the jtestdataaccessor directory should compile, unit test and install the
jtestdataaccessor library into the local Maven repository under ~/.m2.

To create a .jar from the project,
```
$ mvn package
```
can be used. The generated .jar file will be stored in the target/ directory.
Since the .jar does not include AntLR and JUnit, they need to be on the
classpath when using the library. This can be avoided by using
```
$ mvn assembly:assembly
```
which will create target/jtestdataaccessor-bin.jar which includes those two
libraries as well.

Javadoc documentation (and more) can be created by 
```
$ mvn site
```
which creates the target/site/ directory containing an overview of the project
as well as the Javadoc documentation and the cross-referenced source code.


Unfortunately, there might be a problem with the AntLR version used by the
[Maven AntLR plugin](http://maven.apache.org/plugins/maven-antlr-plugin/), if 
the unit tests fail (see below if Maven does not
even execute them because of missing dependencies), this is most likely the
case. A workaround until the real problem is fixed is to modify the file
`~/.m2/repository/org/apache/maven/plugins/maven-antlr-plugin/2.0-beta-1/maven-antlr-plugin-2.0-beta-1.pom`
(or similar, if the problem persists in future releases after adapting the
pom.xml file to use such a release) by changing the AntLR dependency of groupId
= antlr, artifactId = antlrall to groupId = antlr, artifactId = antlr and
version = 2.7.7 - after calling 
```
$ mvn clean
```
to cleanup the generated and compiled sources, 
```
$ mvn install
```
should succeed.

The other problem is the missing support for Junit 4 in the current surefire
plugin, so the unit tests might not be executable at all. This can be solved by
adding the settings from the [Maven Snapshot Guide](http://maven.apache.org/guides/development/guide-plugin-snapshot-repositories.html)
in `~/.m2/settings.xml` - when doing so, the '...'
need to be replaced by `<profiles>` and `</profiles>`, respectively. Now
```
$ mvn -Papache install
```
should finally succeed (-Papache should be required only once, and the snapshot
will be updated periodically afterwards).


##Usage
To use the library, a depedency in the pom.xml of the project which looks like
```xml
<dependency>
    <groupId>ch.ethz.infsec.jtestdataaccessor</groupId>
    <artifactId>jtestdataaccessor</artifactId>
    <version>1.0-SNAPSHOT</version>
    <scope>test</scope>
</dependency>
```
can be used. The library is then added to the classpath during unit testing and
may therefore be used in those tests. Some examples of the usage may be found
in the [src/test/](./src/test/) subtree.


