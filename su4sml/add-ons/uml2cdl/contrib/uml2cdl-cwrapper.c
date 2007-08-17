#include <stdio.h>
#include "uml2cdl-smlffi.h"
#include "uml2cdl-jni.h"

/* void f(char * i,char* o){
  uml2cdl(i,o);
}
*/

JNIEXPORT jint JNICALL Java_Uml2Cdl_uml2cdl (JNIEnv * env,jclass b, jstring input,jstring output){
  const char* in = (*env)->GetStringUTFChars(env, input, 0);
  const char* out = (*env)->GetStringUTFChars(env, output, 0);
  uml2cdl(in,out);
  return 0;
}
