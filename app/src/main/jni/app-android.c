#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>
#include "scheme/scheme.h"

static int  sWindowWidth  = 320;
static int  sWindowHeight = 480;

void Java_foam_starwisp_Scheme_nativeInit(JNIEnv* env, jobject  thiz, jstring logfile)
{
    const char *log_file_str = (*env)->GetStringUTFChars(env, logfile, 0);
    appInit(log_file_str);
}

/* Call to initialize the graphics state */
void Java_foam_starwisp_Scheme_nativeInitGL( JNIEnv*  env )
{
}

void Java_foam_starwisp_Scheme_nativeResize( JNIEnv*  env, jobject  thiz, jint w, jint h )
{
   __android_log_print(ANDROID_LOG_INFO, "SanAngeles", "resize w=%d h=%d", w, h);
}


void Java_foam_starwisp_Scheme_nativeDone(JNIEnv* env)
{
    appDeinit();
}

void Java_foam_starwisp_Scheme_nativeRender( JNIEnv*  env )
{
}


jstring Java_foam_starwisp_Scheme_nativeEval(JNIEnv* env, jobject thiz, jstring code)
{
   const char *native_code = (*env)->GetStringUTFChars(env, code, 0);
   appEval(native_code);
   (*env)->ReleaseStringUTFChars(env, code, native_code);
   if (starwisp_data!=NULL) {
       jstring ret = (*env)->NewStringUTF(env,starwisp_data);
       free(starwisp_data);
       starwisp_data=NULL;
       return ret;
   }
   return (*env)->NewStringUTF(env,"");
}

void Java_foam_starwisp_Scheme_nativeLoadTexture(JNIEnv* env, jobject thiz, jstring texname, jbyteArray arr, jint w, jint h)
{
}

// create the engine and output mix objects
void Java_foam_starwisp_Scheme_createEngine(JNIEnv* env, jclass clazz)
{
}
