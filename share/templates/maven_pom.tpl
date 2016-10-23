@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- an SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// maven_pom.tpl --- 
@// This file is part of su4sml.
@//
@// Copyright (c) 2005-2007, ETH Zurich, Switzerland
@//
@// All rights reserved.
@//
@// Redistribution and use in source and binary forms, with or without
@// modification, are permitted provided that the following conditions are
@// met:
@//
@//     * Redistributions of source code must retain the above copyright
@//       notice, this list of conditions and the following disclaimer.
@//
@//     * Redistributions in binary form must reproduce the above
@//       copyright notice, this list of conditions and the following
@//       disclaimer in the documentation and/or other materials provided
@//       with the distribution.
@//
@//     * Neither the name of the copyright holders nor the names of its
@//       contributors may be used to endorse or promote products derived
@//       from this software without specific prior written permission.
@//
@// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
@// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
@// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
@// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
@// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
@// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
@// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
@// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
@// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
@// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
@// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@///////////////////////////////////////////////////////////////////////////////
@// $Id$

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
