@// Template for a maven Project Object Model (POM) file

@openfileifnotexists generated/pom.xml
<?xml version="1.0"?>
@nl<project>
@nl@tab<modelVersion>4.0.0</modelVersion>
@nl@tab<groupId>groupId<!-- insert a group ID, eg. your company name --></groupId>
@nl@tab<artifactId>artifactId<!-- insert an artifact ID, eg. the package path --></artifactId>
@nl@tab<name>name<!-- insert a name, eg. the project name  --></name>
@nl@tab<version>1.0-SNAPSHOT</version>
@nl@tab<build>
@nl@tab@tab<plugins>
@nl@tab@tab@tab<plugin>
@nl@tab@tab@tab@tab<artifactId>maven-compiler-plugin</artifactId>
@nl@tab@tab@tab@tab<configuration>
@nl@tab@tab@tab@tab@tab<source>1.5</source>
@nl@tab@tab@tab@tab@tab<target>1.5</target>
@nl@tab@tab@tab@tab</configuration>
@nl@tab@tab@tab</plugin>
@nl@tab@tab@tab<plugin>
@nl@tab@tab@tab@tab<groupId>org.apache.maven.plugins</groupId>
@nl@tab@tab@tab@tab<artifactId>maven-surefire-plugin</artifactId>
@nl@tab@tab@tab@tab<version>2.3-SNAPSHOT</version>
@nl@tab@tab@tab@tab<configuration>
@nl@tab@tab@tab@tab@tab<forkMode>pertest</forkMode>
@nl@tab@tab@tab@tab</configuration>
@nl@tab@tab@tab</plugin>
@nl@tab@tab</plugins>
@nl@tab</build>
@nl@tab<dependencies>
@nl@tab@tab<dependency>
@nl@tab@tab@tab<groupId>ch.ethz.infsec.jtestdataaccessor</groupId>
@nl@tab@tab@tab<artifactId>jtestdataaccessor</artifactId>
@nl@tab@tab@tab<version>1.0-SNAPSHOT</version>
@nl@tab@tab@tab<scope>test</scope>
@nl@tab@tab</dependency>
@nl@tab@tab<dependency>
@nl@tab@tab@tab<groupId>tudresden.ocl20</groupId>
@nl@tab@tab@tab<artifactId>stdlib</artifactId>
@nl@tab@tab@tab<version>1.0-SNAPSHOT</version>
@nl@tab@tab@tab<!-- Remove the following line if you try to check conditions in the actual class, not in the unit tests -->
@nl@tab@tab@tab<scope>test</scope>
@nl@tab@tab</dependency>
@nl@tab</dependencies>
@nl@nl</project>
