/* Copyright (C) 1998, 1999, 2000, 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Note: this file must be compilable by the C compiler (for now,
   assuming GNU C is ok).  This means you must never use `//'
   comments, and all C++-specific code must be conditional on
   __cplusplus.  */

#ifndef __GCJ_JNI_H__
#define __GCJ_JNI_H__

#include <gcj/libgcj-config.h>

/* We include <stdio.h> for compatibility with Sun's <jni.h>.  */
#include <stdio.h>

#include <stdarg.h>
#define _Jv_va_list va_list

#ifdef __GCJ_JNI_IMPL__

/* If __GCJ_JNI_IMPL__ is defined, then we assume that we're building
   libgcj itself, and we include headers which taint the namespace
   more than is acceptable for the ordinary JNI user.  */
#include <gcj/javaprims.h>
#include <gcj/array.h>
#include <gnu/gcj/runtime/JNIWeakRef.h>

typedef gnu::gcj::runtime::JNIWeakRef *jweak;

typedef struct _Jv_JNIEnv JNIEnv;
typedef struct _Jv_JavaVM JavaVM;

#define JNI_TRUE true
#define JNI_FALSE false

#else /* __GCJ_JNI_IMPL__ */

# ifdef __GNUC__

/* If we're using gcc, we can use a platform-independent scheme to get
   the right integer types.  */
typedef int    jbyte  __attribute__((__mode__(__QI__)));
typedef int    jshort __attribute__((__mode__(__HI__)));
typedef int    jint   __attribute__((__mode__(__SI__)));
typedef int    jlong  __attribute__((__mode__(__DI__)));
typedef int    jboolean __attribute__((__mode__(__QI__)));
typedef unsigned short jchar __attribute__((__mode__(__HI__)));
typedef float  jfloat;
typedef double jdouble;
typedef jint jsize;

# else /* __GNUC__ */

#  ifdef JV_HAVE_INTTYPES_H

/* If <inttypes.h> is available, we use it.  */

#   include <inttypes.h>

typedef int8_t jbyte;
typedef int16_t jshort;
typedef int32_t jint;
typedef int64_t jlong;
typedef float jfloat;
typedef double jdouble;
typedef jint jsize;
typedef int8_t jboolean;
typedef uint16_t jchar;

#  else /* JV_HAVE_INTTYPES_H */

/* For now, we require either gcc or <inttypes.h>.  If we did more
   work at configure time we could get around this, but right now it
   doesn't seem worth it.  */
#   error jni.h not ported to this platform

#  endif /* JV_HAVE_INTTYPES_H */

# endif /* __GNUC__ */

# ifdef __cplusplus

/* Define dummy classes and then define the JNI types as pointers.  */
struct __jobject {};
struct __jclass : __jobject {};
struct __jstring : __jobject {};
struct __jthrowable : __jobject {};
struct __jweak : __jobject {};
struct __jarray : __jobject {};
struct __jobjectArray : __jarray {};
struct __jbyteArray : __jarray {};
struct __jshortArray : __jarray {};
struct __jintArray : __jarray {};
struct __jlongArray : __jarray {};
struct __jbooleanArray : __jarray {};
struct __jcharArray : __jarray {};
struct __jfloatArray : __jarray {};
struct __jdoubleArray : __jarray {};

typedef __jobject *jobject;
typedef __jclass *jclass;
typedef __jstring *jstring;
typedef __jthrowable *jthrowable;
typedef __jweak *jweak;
typedef __jarray *jarray;
typedef __jobjectArray *jobjectArray;
typedef __jbyteArray *jbyteArray;
typedef __jshortArray *jshortArray;
typedef __jintArray *jintArray;
typedef __jlongArray *jlongArray;
typedef __jbooleanArray *jbooleanArray;
typedef __jcharArray *jcharArray;
typedef __jfloatArray *jfloatArray;
typedef __jdoubleArray *jdoubleArray;

#define JNI_TRUE true
#define JNI_FALSE false

typedef struct _Jv_JNIEnv JNIEnv;
typedef struct _Jv_JavaVM JavaVM;

# else /* __cplusplus */

/* For C, simply define the class types as generic pointers.  */
typedef void *jobject;
typedef jobject jclass;
typedef jobject jstring;
typedef jobject jthrowable;
typedef jobject jweak;
typedef jobject jarray;
typedef jobject jobjectArray;
typedef jobject jbyteArray;
typedef jobject jshortArray;
typedef jobject jintArray;
typedef jobject jlongArray;
typedef jobject jbooleanArray;
typedef jobject jcharArray;
typedef jobject jfloatArray;
typedef jobject jdoubleArray;

#define JNI_TRUE  1
#define JNI_FALSE 0

typedef const struct JNINativeInterface *JNIEnv;
typedef const struct JNIInvokeInterface *JavaVM;

# endif /* __cplusplus */

/* Dummy defines.  */
typedef void *jfieldID;
typedef void *jmethodID;

#endif /* __GCJ_JNI_IMPL__ */

/* Version numbers.  */
#define JNI_VERSION_1_1 0x00010001
#define JNI_VERSION_1_2 0x00010002
#define JNI_VERSION_1_4 0x00010004

/* Used when releasing array elements.  */
#define JNI_COMMIT 1
#define JNI_ABORT  2

/* Error codes */
#define JNI_OK            0
#define JNI_ERR          -1
#define JNI_EDETACHED    -2
#define JNI_EVERSION     -3

/* Linkage and calling conventions. */
#if defined (_WIN32) || defined (__WIN32__) || defined (WIN32)

#define JNIIMPORT        __declspec(dllimport)
#define JNIEXPORT        __declspec(dllexport)

#define JNICALL          __stdcall

/* These defines apply to symbols in libgcj */
#ifdef __GCJ_DLL__
# ifdef __GCJ_JNI_IMPL__
#  define __GCJ_JNIIMPEXP__ JNIEXPORT
# else
#  define __GCJ_JNIIMPEXP__ JNIIMPORT
# endif /* ! __GCJ_JNI_IMPL__ */
#else /* ! __GCJ_DLL__ */
# define __GCJ_JNIIMPEXP__
#endif /*  __GCJ_DLL__ */

#else /* !( _WIN32 || __WIN32__ || WIN32) */

#define JNIIMPORT
#define JNIEXPORT
#define JNICALL
#define __GCJ_JNIIMPEXP__

#endif /* !( _WIN32 || __WIN32__ || WIN32) */

 
#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/* These functions might be defined in libraries which we load; the
   JNI implementation calls them at the appropriate times.  */
extern JNIEXPORT jint JNICALL JNI_OnLoad (JavaVM *, void *);
extern JNIEXPORT void JNICALL JNI_OnUnload (JavaVM *, void *);

/* These functions are called by user code to start using the
   invocation API.  */
extern __GCJ_JNIIMPEXP__ jint JNICALL
JNI_GetDefaultJavaVMInitArgs (void *);

extern __GCJ_JNIIMPEXP__ jint JNICALL
JNI_CreateJavaVM (JavaVM **, void **, void *);

extern __GCJ_JNIIMPEXP__ jint JNICALL
JNI_GetCreatedJavaVMs(JavaVM **, jsize, jsize *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

typedef union jvalue
{
  jboolean z;
  jbyte    b;
  jchar    c;
  jshort   s;
  jint     i;
  jlong    j;
  jfloat   f;
  jdouble  d;
  jobject  l;
} jvalue;

#ifdef __cplusplus
typedef void * (*_Jv_func) (...);
#else
typedef void * (*_Jv_func) ();
#endif

/* This structure is used when registering native methods.  */
typedef struct
{
  char *name;
  char *signature;
  void *fnPtr;			/* Sigh.  */
} JNINativeMethod;

struct JNINativeInterface
{
  _Jv_func reserved0;
  _Jv_func reserved1;
  _Jv_func reserved2;
  _Jv_func reserved3;

  jint     (JNICALL *GetVersion)                   (JNIEnv *);
  jclass   (JNICALL *DefineClass)                  (JNIEnv *, const char *,
						    jobject, const jbyte *,
						    jsize);
  jclass   (JNICALL *FindClass)                    (JNIEnv *, const char *);

  jmethodID (JNICALL *FromReflectedMethod)	   (JNIEnv *, jobject);
  jfieldID  (JNICALL *FromReflectedField)	   (JNIEnv *, jobject);
  jobject   (JNICALL *ToReflectedMethod)	   (JNIEnv *, jclass,
						    jmethodID, jboolean);

  jclass   (JNICALL *GetSuperclass)                (JNIEnv *, jclass);
  jboolean (JNICALL *IsAssignableFrom)             (JNIEnv *, jclass, jclass);

  jobject  (JNICALL *ToReflectedField)		   (JNIEnv *, jclass, jfieldID,
                                                    jboolean);

  jint     (JNICALL *Throw)                        (JNIEnv *, jthrowable);
  jint     (JNICALL *ThrowNew)                     (JNIEnv *, jclass, 
                                                    const char *);
  jthrowable (JNICALL *ExceptionOccurred)          (JNIEnv *);
  void     (JNICALL *ExceptionDescribe)            (JNIEnv *);
  void     (JNICALL *ExceptionClear)               (JNIEnv *);
  void     (JNICALL *FatalError)                   (JNIEnv *, const char *);

  jint     (JNICALL *PushLocalFrame)		   (JNIEnv *, jint);
  jobject  (JNICALL *PopLocalFrame)		   (JNIEnv *, jobject);

  jobject  (JNICALL *NewGlobalRef)                 (JNIEnv *, jobject);
  void     (JNICALL *DeleteGlobalRef)              (JNIEnv *, jobject);
  void     (JNICALL *DeleteLocalRef)               (JNIEnv *, jobject);
  jboolean (JNICALL *IsSameObject)                 (JNIEnv *, jobject, 
                                                    jobject);

  jobject  (JNICALL *NewLocalRef)		   (JNIEnv *, jobject);
  jint     (JNICALL *EnsureLocalCapacity)	   (JNIEnv *, jint);

  jobject  (JNICALL *AllocObject)                  (JNIEnv *, jclass);
  jobject (JNICALL *NewObject)			   (JNIEnv *, jclass, 
                                                    jmethodID, ...);
  jobject (JNICALL *NewObjectV)			   (JNIEnv *, jclass, 
                                                    jmethodID, _Jv_va_list);
  jobject (JNICALL *NewObjectA)			   (JNIEnv *, jclass, 
                                                    jmethodID, jvalue *);

  jclass   (JNICALL *GetObjectClass)               (JNIEnv *, jobject);
  jboolean (JNICALL *IsInstanceOf)                 (JNIEnv *, jobject, jclass);
  jmethodID (JNICALL *GetMethodID)                 (JNIEnv *, jclass, 
                                                    const char *, const char *);

  jobject (JNICALL *CallObjectMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jobject (JNICALL *CallObjectMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jobject (JNICALL *CallObjectMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jboolean (JNICALL *CallBooleanMethod)	   (JNIEnv *, jobject, jmethodID,
                                            ...);
  jboolean (JNICALL *CallBooleanMethodV)   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jboolean (JNICALL *CallBooleanMethodA)   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jbyte (JNICALL *CallByteMethod)   (JNIEnv *, jobject, jmethodID, ...);
  jbyte (JNICALL *CallByteMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jbyte (JNICALL *CallByteMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jchar (JNICALL *CallCharMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jchar (JNICALL *CallCharMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jchar (JNICALL *CallCharMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jshort (JNICALL *CallShortMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jshort (JNICALL *CallShortMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jshort (JNICALL *CallShortMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jint 	(JNICALL *CallIntMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jint 	(JNICALL *CallIntMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jint 	(JNICALL *CallIntMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jlong (JNICALL *CallLongMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jlong (JNICALL *CallLongMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jlong (JNICALL *CallLongMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jfloat (JNICALL *CallFloatMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jfloat (JNICALL *CallFloatMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jfloat (JNICALL *CallFloatMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  jdouble (JNICALL *CallDoubleMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jdouble (JNICALL *CallDoubleMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  jdouble (JNICALL *CallDoubleMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);
  void  (JNICALL *CallVoidMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  void  (JNICALL *CallVoidMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            _Jv_va_list);
  void  (JNICALL *CallVoidMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            jvalue *);

  jobject   (JNICALL *CallNonvirtualObjectMethod)  (JNIEnv *, jobject, jclass,
                                                    jmethodID, ...);
  jobject   (JNICALL *CallNonvirtualObjectMethodV) (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jobject   (JNICALL *CallNonvirtualObjectMethodA) (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jboolean  (JNICALL *CallNonvirtualBooleanMethod) (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jboolean  (JNICALL *CallNonvirtualBooleanMethodV) (JNIEnv *, jobject, jclass,
					             jmethodID, _Jv_va_list);
  jboolean  (JNICALL *CallNonvirtualBooleanMethodA) (JNIEnv *, jobject, jclass,
					             jmethodID, jvalue *);
  jbyte     (JNICALL *CallNonvirtualByteMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jbyte     (JNICALL *CallNonvirtualByteMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jbyte     (JNICALL *CallNonvirtualByteMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jchar     (JNICALL *CallNonvirtualCharMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jchar     (JNICALL *CallNonvirtualCharMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jchar     (JNICALL *CallNonvirtualCharMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jshort    (JNICALL *CallNonvirtualShortMethod)   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jshort    (JNICALL *CallNonvirtualShortMethodV)  (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jshort    (JNICALL *CallNonvirtualShortMethodA)  (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jint 	    (JNICALL *CallNonvirtualIntMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jint 	    (JNICALL *CallNonvirtualIntMethodV)	   (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jint 	    (JNICALL *CallNonvirtualIntMethodA)	   (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jlong     (JNICALL *CallNonvirtualLongMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jlong     (JNICALL *CallNonvirtualLongMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jlong     (JNICALL *CallNonvirtualLongMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jfloat    (JNICALL *CallNonvirtualFloatMethod)   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jfloat    (JNICALL *CallNonvirtualFloatMethodV)  (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jfloat    (JNICALL *CallNonvirtualFloatMethodA)  (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  jdouble   (JNICALL *CallNonvirtualDoubleMethod)  (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jdouble   (JNICALL *CallNonvirtualDoubleMethodV) (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  jdouble   (JNICALL *CallNonvirtualDoubleMethodA) (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);
  void      (JNICALL *CallNonvirtualVoidMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  void      (JNICALL *CallNonvirtualVoidMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, _Jv_va_list);
  void      (JNICALL *CallNonvirtualVoidMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, jvalue *);

  jfieldID  (JNICALL *GetFieldID)          (JNIEnv *, jclass, const char *,
					    const char *);

  jobject  (JNICALL *GetObjectField)       (JNIEnv *, jobject, jfieldID);
  jboolean (JNICALL *GetBooleanField)      (JNIEnv *, jobject, jfieldID);
  jbyte    (JNICALL *GetByteField)         (JNIEnv *, jobject, jfieldID);
  jchar    (JNICALL *GetCharField)         (JNIEnv *, jobject, jfieldID);
  jshort   (JNICALL *GetShortField)        (JNIEnv *, jobject, jfieldID);
  jint     (JNICALL *GetIntField)          (JNIEnv *, jobject, jfieldID);
  jlong    (JNICALL *GetLongField)         (JNIEnv *, jobject, jfieldID);
  jfloat   (JNICALL *GetFloatField)        (JNIEnv *, jobject, jfieldID);
  jdouble  (JNICALL *GetDoubleField)       (JNIEnv *, jobject, jfieldID);

  void	(JNICALL *SetObjectField)	   (JNIEnv *, jobject,
					    jfieldID, jobject);
  void	(JNICALL *SetBooleanField)	   (JNIEnv *, jobject,
					    jfieldID, jboolean);
  void	(JNICALL *SetByteField)		   (JNIEnv *, jobject,
					    jfieldID, jbyte);
  void	(JNICALL *SetCharField)		   (JNIEnv *, jobject,
					    jfieldID, jchar);
  void	(JNICALL *SetShortField)	   (JNIEnv *, jobject,
					    jfieldID, jshort);
  void	(JNICALL *SetIntField)		   (JNIEnv *, jobject,
					    jfieldID, jint);
  void	(JNICALL *SetLongField)		   (JNIEnv *, jobject,
					    jfieldID, jlong);
  void	(JNICALL *SetFloatField)	   (JNIEnv *, jobject,
					    jfieldID, jfloat);
  void	(JNICALL *SetDoubleField)	   (JNIEnv *, jobject,
					    jfieldID, jdouble);

  jmethodID (JNICALL *GetStaticMethodID)   (JNIEnv *, jclass, const char *,
					    const char *);

  jobject  (JNICALL *CallStaticObjectMethod)  (JNIEnv *, jclass, jmethodID,
					       ...);
  jobject  (JNICALL *CallStaticObjectMethodV) (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jobject  (JNICALL *CallStaticObjectMethodA) (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jboolean (JNICALL *CallStaticBooleanMethod) (JNIEnv *, jclass, jmethodID,
					       ...);
  jboolean (JNICALL *CallStaticBooleanMethodV) (JNIEnv *, jclass, jmethodID,
					        _Jv_va_list);
  jboolean (JNICALL *CallStaticBooleanMethodA) (JNIEnv *, jclass, jmethodID,
					        jvalue *);
  jbyte	   (JNICALL *CallStaticByteMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jbyte    (JNICALL *CallStaticByteMethodV)   (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jbyte    (JNICALL *CallStaticByteMethodA)   (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jchar    (JNICALL *CallStaticCharMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jchar    (JNICALL *CallStaticCharMethodV)   (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jchar    (JNICALL *CallStaticCharMethodA)   (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jshort   (JNICALL *CallStaticShortMethod)   (JNIEnv *, jclass, jmethodID,
					       ...);
  jshort   (JNICALL *CallStaticShortMethodV)  (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jshort   (JNICALL *CallStaticShortMethodA)  (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jint 	   (JNICALL *CallStaticIntMethod)     (JNIEnv *, jclass, jmethodID,
					       ...);
  jint 	   (JNICALL *CallStaticIntMethodV)    (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jint 	   (JNICALL *CallStaticIntMethodA)    (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jlong    (JNICALL *CallStaticLongMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jlong    (JNICALL *CallStaticLongMethodV)   (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jlong    (JNICALL *CallStaticLongMethodA)   (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jfloat   (JNICALL *CallStaticFloatMethod)   (JNIEnv *, jclass, jmethodID,
					       ...);
  jfloat   (JNICALL *CallStaticFloatMethodV)  (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jfloat   (JNICALL *CallStaticFloatMethodA)  (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  jdouble  (JNICALL *CallStaticDoubleMethod)  (JNIEnv *, jclass, jmethodID,
					       ...);
  jdouble  (JNICALL *CallStaticDoubleMethodV) (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  jdouble  (JNICALL *CallStaticDoubleMethodA) (JNIEnv *, jclass, jmethodID,
					       jvalue *);
  void     (JNICALL *CallStaticVoidMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  void     (JNICALL *CallStaticVoidMethodV)   (JNIEnv *, jclass, jmethodID,
					       _Jv_va_list);
  void     (JNICALL *CallStaticVoidMethodA)   (JNIEnv *, jclass, jmethodID,
					       jvalue *);

  jfieldID (JNICALL *GetStaticFieldID)        (JNIEnv *, jclass, const char *,
					       const char *);

  jobject  (JNICALL *GetStaticObjectField)    (JNIEnv *, jclass, jfieldID);
  jboolean (JNICALL *GetStaticBooleanField)   (JNIEnv *, jclass, jfieldID);
  jbyte	   (JNICALL *GetStaticByteField)      (JNIEnv *, jclass, jfieldID);
  jchar	   (JNICALL *GetStaticCharField)      (JNIEnv *, jclass, jfieldID);
  jshort   (JNICALL *GetStaticShortField)     (JNIEnv *, jclass, jfieldID);
  jint	   (JNICALL *GetStaticIntField)	      (JNIEnv *, jclass, jfieldID);
  jlong	   (JNICALL *GetStaticLongField)      (JNIEnv *, jclass, jfieldID);
  jfloat   (JNICALL *GetStaticFloatField)     (JNIEnv *, jclass, jfieldID);
  jdouble  (JNICALL *GetStaticDoubleField)    (JNIEnv *, jclass, jfieldID);

  void 	(JNICALL *SetStaticObjectField)	   (JNIEnv *, jclass,
					    jfieldID, jobject);
  void 	(JNICALL *SetStaticBooleanField)   (JNIEnv *, jclass,
					    jfieldID, jboolean);
  void 	(JNICALL *SetStaticByteField)	   (JNIEnv *, jclass,
					    jfieldID, jbyte);
  void 	(JNICALL *SetStaticCharField)	   (JNIEnv *, jclass,
					    jfieldID, jchar);
  void 	(JNICALL *SetStaticShortField)	   (JNIEnv *, jclass,
					    jfieldID, jshort);
  void 	(JNICALL *SetStaticIntField)	   (JNIEnv *, jclass,
					    jfieldID, jint);
  void 	(JNICALL *SetStaticLongField)	   (JNIEnv *, jclass,
					    jfieldID, jlong);
  void 	(JNICALL *SetStaticFloatField)	   (JNIEnv *, jclass,
					    jfieldID, jfloat);
  void 	(JNICALL *SetStaticDoubleField)	   (JNIEnv *, jclass,
					    jfieldID, jdouble);

  jstring  (JNICALL *NewString)            (JNIEnv *, const jchar *, jsize);
  jsize    (JNICALL *GetStringLength)      (JNIEnv *, jstring);
  const jchar * (JNICALL *GetStringChars)  (JNIEnv *, jstring, jboolean *);
  void     (JNICALL *ReleaseStringChars)   (JNIEnv *, jstring, const jchar *);
  jstring  (JNICALL *NewStringUTF)         (JNIEnv *, const char *);
  jsize    (JNICALL *GetStringUTFLength)   (JNIEnv *, jstring);
  const char * (JNICALL *GetStringUTFChars) (JNIEnv *, jstring, jboolean *);
  void     (JNICALL *ReleaseStringUTFChars) (JNIEnv *, jstring, const char *);
  jsize    (JNICALL *GetArrayLength)       (JNIEnv *, jarray);
  jarray   (JNICALL *NewObjectArray)       (JNIEnv *, jsize, jclass, jobject);
  jobject  (JNICALL *GetObjectArrayElement) (JNIEnv *, jobjectArray, jsize);
  void     (JNICALL *SetObjectArrayElement) (JNIEnv *, jobjectArray, jsize,
					     jobject);

  jbooleanArray (JNICALL *NewBooleanArray)	   (JNIEnv *, jsize);
  jbyteArray    (JNICALL *NewByteArray)		   (JNIEnv *, jsize);
  jcharArray    (JNICALL *NewCharArray)		   (JNIEnv *, jsize);
  jshortArray   (JNICALL *NewShortArray)	   (JNIEnv *, jsize);
  jintArray     (JNICALL *NewIntArray)		   (JNIEnv *, jsize);
  jlongArray    (JNICALL *NewLongArray)		   (JNIEnv *, jsize);
  jfloatArray   (JNICALL *NewFloatArray)	   (JNIEnv *, jsize);
  jdoubleArray  (JNICALL *NewDoubleArray)	   (JNIEnv *, jsize);

  jboolean *	(JNICALL *GetBooleanArrayElements) (JNIEnv *, jbooleanArray,
					            jboolean *);
  jbyte *	(JNICALL *GetByteArrayElements)	   (JNIEnv *, jbyteArray,
					            jboolean *);
  jchar *	(JNICALL *GetCharArrayElements)	   (JNIEnv *, jcharArray,
					            jboolean *);
  jshort *	(JNICALL *GetShortArrayElements)   (JNIEnv *, jshortArray,
					            jboolean *);
  jint *	(JNICALL *GetIntArrayElements)	   (JNIEnv *, jintArray,
					            jboolean *);
  jlong *	(JNICALL *GetLongArrayElements)	   (JNIEnv *, jlongArray,
					            jboolean *);
  jfloat *	(JNICALL *GetFloatArrayElements)   (JNIEnv *, jfloatArray,
					            jboolean *);
  jdouble *	(JNICALL *GetDoubleArrayElements)  (JNIEnv *, jdoubleArray,
					            jboolean *);

  void		(JNICALL *ReleaseBooleanArrayElements) (JNIEnv *, jbooleanArray,
						        jboolean *, jint);
  void		(JNICALL *ReleaseByteArrayElements)    (JNIEnv *, jbyteArray,
					                jbyte *, jint);
  void		(JNICALL *ReleaseCharArrayElements)    (JNIEnv *, jcharArray,
						        jchar *, jint);
  void		(JNICALL *ReleaseShortArrayElements)   (JNIEnv *, jshortArray,
						        jshort *, jint);
  void		(JNICALL *ReleaseIntArrayElements)     (JNIEnv *, jintArray,
						        jint *, jint);
  void		(JNICALL *ReleaseLongArrayElements)    (JNIEnv *, jlongArray,
						        jlong *, jint);
  void		(JNICALL *ReleaseFloatArrayElements)   (JNIEnv *, jfloatArray,
						        jfloat *, jint);
  void		(JNICALL *ReleaseDoubleArrayElements)  (JNIEnv *, jdoubleArray,
						        jdouble *, jint);

  void 		(JNICALL *GetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					            jsize, jsize, jboolean *);
  void 		(JNICALL *GetByteArrayRegion)	   (JNIEnv *, jbyteArray,
					            jsize, jsize, jbyte *);
  void 		(JNICALL *GetCharArrayRegion)	   (JNIEnv *, jcharArray,
					            jsize, jsize, jchar *);
  void 		(JNICALL *GetShortArrayRegion)	   (JNIEnv *, jshortArray,
					            jsize, jsize, jshort *);
  void 		(JNICALL *GetIntArrayRegion)	   (JNIEnv *, jintArray,
					            jsize, jsize, jint *);
  void 		(JNICALL *GetLongArrayRegion)	   (JNIEnv *, jlongArray,
					            jsize, jsize, jlong *);
  void 		(JNICALL *GetFloatArrayRegion)	   (JNIEnv *, jfloatArray,
					            jsize, jsize, jfloat *);
  void 		(JNICALL *GetDoubleArrayRegion)	   (JNIEnv *, jdoubleArray,
					            jsize, jsize, jdouble *);

  void 		(JNICALL *SetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					            jsize, jsize, jboolean *);
  void 		(JNICALL *SetByteArrayRegion)	   (JNIEnv *, jbyteArray,
					            jsize, jsize, jbyte *);
  void 		(JNICALL *SetCharArrayRegion)	   (JNIEnv *, jcharArray,
					            jsize, jsize, jchar *);
  void 		(JNICALL *SetShortArrayRegion)	   (JNIEnv *, jshortArray,
					            jsize, jsize, jshort *);
  void 		(JNICALL *SetIntArrayRegion)	   (JNIEnv *, jintArray,
					            jsize, jsize, jint *);
  void 		(JNICALL *SetLongArrayRegion)	   (JNIEnv *, jlongArray,
					            jsize, jsize, jlong *);
  void 		(JNICALL *SetFloatArrayRegion)	   (JNIEnv *, jfloatArray,
					            jsize, jsize, jfloat *);
  void 		(JNICALL *SetDoubleArrayRegion)	   (JNIEnv *, jdoubleArray,
					            jsize, jsize, jdouble *);

  jint     (JNICALL *RegisterNatives)              (JNIEnv *, jclass,
					            const JNINativeMethod *, 
						    jint);
  jint     (JNICALL *UnregisterNatives)            (JNIEnv *, jclass);
  jint     (JNICALL *MonitorEnter)                 (JNIEnv *, jobject);
  jint     (JNICALL *MonitorExit)                  (JNIEnv *, jobject);
  jint     (JNICALL *GetJavaVM)                    (JNIEnv *, JavaVM **);

  void	   (JNICALL *GetStringRegion)	           (JNIEnv *, jstring, jsize,
					            jsize, jchar *);
  void     (JNICALL *GetStringUTFRegion)	   (JNIEnv *, jstring, jsize,
					            jsize, char *);

  void * (JNICALL *GetPrimitiveArrayCritical)      (JNIEnv *, jarray, 
                                                    jboolean *);
  void   (JNICALL *ReleasePrimitiveArrayCritical)  (JNIEnv *, jarray, void *, 
                                                    jint);

  const jchar * (JNICALL *GetStringCritical)       (JNIEnv *, jstring, 
                                                    jboolean *);
  void          (JNICALL *ReleaseStringCritical)   (JNIEnv *, jstring, 
                                                    const jchar *);

  jweak  (JNICALL *NewWeakGlobalRef)               (JNIEnv *, jobject);
  void   (JNICALL *DeleteWeakGlobalRef)            (JNIEnv *, jweak);

  jboolean	(JNICALL *ExceptionCheck)	   (JNIEnv *);

  jobject (JNICALL *NewDirectByteBuffer)           (JNIEnv *, void *, jlong);
  void *  (JNICALL *GetDirectBufferAddress)        (JNIEnv *, jobject);
  jlong   (JNICALL *GetDirectBufferCapacity)       (JNIEnv *, jobject);
};

#ifdef __cplusplus

class _Jv_JNIEnv
{
public:
  /* The method table.  */
  struct JNINativeInterface *p;

  /* This is ugly, but we must live with it.  */
#ifndef __GCJ_JNI_IMPL__
private:
#endif
  /* The current exception.  */
  jthrowable ex;

  /* The class of the current native method.  */
  jclass klass;

  /* The chain of local frames.  */
  struct _Jv_JNI_LocalFrame *locals;

public:
  jint GetVersion ()
  { return p->GetVersion (this); }

  jclass DefineClass (const char *name, jobject obj0, const jbyte * val1,
		      jsize val2)
  { return p->DefineClass (this, name, obj0, val1, val2); }

  jclass FindClass (const char * val0)
  { return p->FindClass (this, val0); }

  jmethodID FromReflectedMethod (jobject obj0)
  { return p->FromReflectedMethod (this, obj0); }

  jfieldID FromReflectedField (jobject obj0)
  { return p->FromReflectedField (this, obj0); }

  jobject ToReflectedMethod (jclass cl0, jmethodID meth1, jboolean val2)
  { return p->ToReflectedMethod (this, cl0, meth1, val2); }

  jclass GetSuperclass (jclass cl0)
  { return p->GetSuperclass (this, cl0); }

  jboolean IsAssignableFrom (jclass cl0, jclass cl1)
  { return p->IsAssignableFrom (this, cl0, cl1); }

  jobject ToReflectedField (jclass cl0, jfieldID fld1, jboolean val2)
  { return p->ToReflectedField (this, cl0, fld1, val2); }

  jint Throw (jthrowable val0)
  { return p->Throw (this, val0); }

  jint ThrowNew (jclass cl0, const char * val1)
  { return p->ThrowNew (this, cl0, val1); }

  jthrowable ExceptionOccurred ()
  { return p->ExceptionOccurred (this); }

  void ExceptionDescribe ()
  { p->ExceptionDescribe (this); }

  void ExceptionClear ()
  { p->ExceptionClear (this); }

  void FatalError (const char * val0)
  { p->FatalError (this, val0); }

  jint PushLocalFrame (jint val0)
  { return p->PushLocalFrame (this, val0); }

  jobject PopLocalFrame (jobject obj0)
  { return p->PopLocalFrame (this, obj0); }

  jobject NewGlobalRef (jobject obj0)
  { return p->NewGlobalRef (this, obj0); }

  void DeleteGlobalRef (jobject obj0)
  { p->DeleteGlobalRef (this, obj0); }

  void DeleteLocalRef (jobject obj0)
  { p->DeleteLocalRef (this, obj0); }

  jboolean IsSameObject (jobject obj0, jobject obj1)
  { return p->IsSameObject (this, obj0, obj1); }

  jobject NewLocalRef (jobject obj0)
  { return p->NewLocalRef (this, obj0); }

  jint EnsureLocalCapacity (jint val0)
  { return p->EnsureLocalCapacity (this, val0); }

  jobject AllocObject (jclass cl0)
  { return p->AllocObject (this, cl0); }

  jobject NewObject (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->NewObjectV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject NewObjectV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->NewObjectV (this, cl0, meth1, val2); }

  jobject NewObjectA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->NewObjectA (this, cl0, meth1, val2); }

  jclass GetObjectClass (jobject obj0)
  { return p->GetObjectClass (this, obj0); }

  jboolean IsInstanceOf (jobject obj0, jclass cl1)
  { return p->IsInstanceOf (this, obj0, cl1); }

  jmethodID GetMethodID (jclass cl0, const char * val1, const char * val2)
  { return p->GetMethodID (this, cl0, val1, val2); }

  jobject CallObjectMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->CallObjectMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallObjectMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallObjectMethodV (this, obj0, meth1, val2); }

  jobject CallObjectMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallObjectMethodA (this, obj0, meth1, val2); }

  jboolean CallBooleanMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jboolean result = p->CallBooleanMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallBooleanMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallBooleanMethodV (this, obj0, meth1, val2); }

  jboolean CallBooleanMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallBooleanMethodA (this, obj0, meth1, val2); }

  jbyte CallByteMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jbyte result = p->CallByteMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallByteMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallByteMethodV (this, obj0, meth1, val2); }

  jbyte CallByteMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallByteMethodA (this, obj0, meth1, val2); }

  jchar CallCharMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jchar result = p->CallCharMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallCharMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallCharMethodV (this, obj0, meth1, val2); }

  jchar CallCharMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallCharMethodA (this, obj0, meth1, val2); }

  jshort CallShortMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jshort result = p->CallShortMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallShortMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallShortMethodV (this, obj0, meth1, val2); }

  jshort CallShortMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallShortMethodA (this, obj0, meth1, val2); }

  jint CallIntMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jint result = p->CallIntMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallIntMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallIntMethodV (this, obj0, meth1, val2); }

  jint CallIntMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallIntMethodA (this, obj0, meth1, val2); }

  jlong CallLongMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jlong result = p->CallLongMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallLongMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallLongMethodV (this, obj0, meth1, val2); }

  jlong CallLongMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallLongMethodA (this, obj0, meth1, val2); }

  jfloat CallFloatMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jfloat result = p->CallFloatMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallFloatMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallFloatMethodV (this, obj0, meth1, val2); }

  jfloat CallFloatMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallFloatMethodA (this, obj0, meth1, val2); }

  jdouble CallDoubleMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jdouble result = p->CallDoubleMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallDoubleMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallDoubleMethodV (this, obj0, meth1, val2); }

  jdouble CallDoubleMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallDoubleMethodA (this, obj0, meth1, val2); }

  void CallVoidMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    p->CallVoidMethodV (this, obj0, meth1, args);
    va_end (args);
  }

  void CallVoidMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { p->CallVoidMethodV (this, obj0, meth1, val2); }

  void CallVoidMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { p->CallVoidMethodA (this, obj0, meth1, val2); }

  jobject CallNonvirtualObjectMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jobject result = p->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jobject CallNonvirtualObjectMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, val3); }

  jobject CallNonvirtualObjectMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualObjectMethodA (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jboolean result = p->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jboolean CallNonvirtualBooleanMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualBooleanMethodA (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jbyte result = p->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jbyte CallNonvirtualByteMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualByteMethodA (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jchar result = p->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jchar CallNonvirtualCharMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualCharMethodA (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jshort result = p->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jshort CallNonvirtualShortMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualShortMethodA (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jint result = p->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jint CallNonvirtualIntMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualIntMethodA (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jlong result = p->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jlong CallNonvirtualLongMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualLongMethodA (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jfloat result = p->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jfloat CallNonvirtualFloatMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualFloatMethodA (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jdouble result = p->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jdouble CallNonvirtualDoubleMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualDoubleMethodA (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    p->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
  }

  void CallNonvirtualVoidMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { p->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { p->CallNonvirtualVoidMethodA (this, obj0, cl1, meth2, val3); }

  jfieldID GetFieldID (jclass cl0, const char * val1, const char * val2)
  { return p->GetFieldID (this, cl0, val1, val2); }

  jobject GetObjectField (jobject obj0, jfieldID fld1)
  { return p->GetObjectField (this, obj0, fld1); }

  jboolean GetBooleanField (jobject obj0, jfieldID fld1)
  { return p->GetBooleanField (this, obj0, fld1); }

  jbyte GetByteField (jobject obj0, jfieldID fld1)
  { return p->GetByteField (this, obj0, fld1); }

  jchar GetCharField (jobject obj0, jfieldID fld1)
  { return p->GetCharField (this, obj0, fld1); }

  jshort GetShortField (jobject obj0, jfieldID fld1)
  { return p->GetShortField (this, obj0, fld1); }

  jint GetIntField (jobject obj0, jfieldID fld1)
  { return p->GetIntField (this, obj0, fld1); }

  jlong GetLongField (jobject obj0, jfieldID fld1)
  { return p->GetLongField (this, obj0, fld1); }

  jfloat GetFloatField (jobject obj0, jfieldID fld1)
  { return p->GetFloatField (this, obj0, fld1); }

  jdouble GetDoubleField (jobject obj0, jfieldID fld1)
  { return p->GetDoubleField (this, obj0, fld1); }

  void SetObjectField (jobject obj0, jfieldID fld1, jobject obj2)
  { p->SetObjectField (this, obj0, fld1, obj2); }

  void SetBooleanField (jobject obj0, jfieldID fld1, jboolean val2)
  { p->SetBooleanField (this, obj0, fld1, val2); }

  void SetByteField (jobject obj0, jfieldID fld1, jbyte val2)
  { p->SetByteField (this, obj0, fld1, val2); }

  void SetCharField (jobject obj0, jfieldID fld1, jchar val2)
  { p->SetCharField (this, obj0, fld1, val2); }

  void SetShortField (jobject obj0, jfieldID fld1, jshort val2)
  { p->SetShortField (this, obj0, fld1, val2); }

  void SetIntField (jobject obj0, jfieldID fld1, jint val2)
  { p->SetIntField (this, obj0, fld1, val2); }

  void SetLongField (jobject obj0, jfieldID fld1, jlong val2)
  { p->SetLongField (this, obj0, fld1, val2); }

  void SetFloatField (jobject obj0, jfieldID fld1, jfloat val2)
  { p->SetFloatField (this, obj0, fld1, val2); }

  void SetDoubleField (jobject obj0, jfieldID fld1, jdouble val2)
  { p->SetDoubleField (this, obj0, fld1, val2); }

  jmethodID GetStaticMethodID (jclass cl0, const char * val1, const char * val2)
  { return p->GetStaticMethodID (this, cl0, val1, val2); }

  jobject CallStaticObjectMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->CallStaticObjectMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallStaticObjectMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticObjectMethodV (this, cl0, meth1, val2); }

  jobject CallStaticObjectMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticObjectMethodA (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jboolean result = p->CallStaticBooleanMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallStaticBooleanMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticBooleanMethodV (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticBooleanMethodA (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jbyte result = p->CallStaticByteMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallStaticByteMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticByteMethodV (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticByteMethodA (this, cl0, meth1, val2); }

  jchar CallStaticCharMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jchar result = p->CallStaticCharMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallStaticCharMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticCharMethodV (this, cl0, meth1, val2); }

  jchar CallStaticCharMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticCharMethodA (this, cl0, meth1, val2); }

  jshort CallStaticShortMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jshort result = p->CallStaticShortMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallStaticShortMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticShortMethodV (this, cl0, meth1, val2); }

  jshort CallStaticShortMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticShortMethodA (this, cl0, meth1, val2); }

  jint CallStaticIntMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jint result = p->CallStaticIntMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallStaticIntMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticIntMethodV (this, cl0, meth1, val2); }

  jint CallStaticIntMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticIntMethodA (this, cl0, meth1, val2); }

  jlong CallStaticLongMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jlong result = p->CallStaticLongMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallStaticLongMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticLongMethodV (this, cl0, meth1, val2); }

  jlong CallStaticLongMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticLongMethodA (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jfloat result = p->CallStaticFloatMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallStaticFloatMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticFloatMethodV (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticFloatMethodA (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jdouble result = p->CallStaticDoubleMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallStaticDoubleMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticDoubleMethodV (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticDoubleMethodA (this, cl0, meth1, val2); }

  void CallStaticVoidMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    p->CallStaticVoidMethodV (this, cl0, meth1, args);
    va_end (args);
  }

  void CallStaticVoidMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { p->CallStaticVoidMethodV (this, cl0, meth1, val2); }

  void CallStaticVoidMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { p->CallStaticVoidMethodA (this, cl0, meth1, val2); }

  jfieldID GetStaticFieldID (jclass cl0, const char * val1, const char * val2)
  { return p->GetStaticFieldID (this, cl0, val1, val2); }

  jobject GetStaticObjectField (jclass cl0, jfieldID fld1)
  { return p->GetStaticObjectField (this, cl0, fld1); }

  jboolean GetStaticBooleanField (jclass cl0, jfieldID fld1)
  { return p->GetStaticBooleanField (this, cl0, fld1); }

  jbyte GetStaticByteField (jclass cl0, jfieldID fld1)
  { return p->GetStaticByteField (this, cl0, fld1); }

  jchar GetStaticCharField (jclass cl0, jfieldID fld1)
  { return p->GetStaticCharField (this, cl0, fld1); }

  jshort GetStaticShortField (jclass cl0, jfieldID fld1)
  { return p->GetStaticShortField (this, cl0, fld1); }

  jint GetStaticIntField (jclass cl0, jfieldID fld1)
  { return p->GetStaticIntField (this, cl0, fld1); }

  jlong GetStaticLongField (jclass cl0, jfieldID fld1)
  { return p->GetStaticLongField (this, cl0, fld1); }

  jfloat GetStaticFloatField (jclass cl0, jfieldID fld1)
  { return p->GetStaticFloatField (this, cl0, fld1); }

  jdouble GetStaticDoubleField (jclass cl0, jfieldID fld1)
  { return p->GetStaticDoubleField (this, cl0, fld1); }

  void SetStaticObjectField (jclass cl0, jfieldID fld1, jobject obj2)
  { p->SetStaticObjectField (this, cl0, fld1, obj2); }

  void SetStaticBooleanField (jclass cl0, jfieldID fld1, jboolean val2)
  { p->SetStaticBooleanField (this, cl0, fld1, val2); }

  void SetStaticByteField (jclass cl0, jfieldID fld1, jbyte val2)
  { p->SetStaticByteField (this, cl0, fld1, val2); }

  void SetStaticCharField (jclass cl0, jfieldID fld1, jchar val2)
  { p->SetStaticCharField (this, cl0, fld1, val2); }

  void SetStaticShortField (jclass cl0, jfieldID fld1, jshort val2)
  { p->SetStaticShortField (this, cl0, fld1, val2); }

  void SetStaticIntField (jclass cl0, jfieldID fld1, jint val2)
  { p->SetStaticIntField (this, cl0, fld1, val2); }

  void SetStaticLongField (jclass cl0, jfieldID fld1, jlong val2)
  { p->SetStaticLongField (this, cl0, fld1, val2); }

  void SetStaticFloatField (jclass cl0, jfieldID fld1, jfloat val2)
  { p->SetStaticFloatField (this, cl0, fld1, val2); }

  void SetStaticDoubleField (jclass cl0, jfieldID fld1, jdouble val2)
  { p->SetStaticDoubleField (this, cl0, fld1, val2); }

  jstring NewString (const jchar * val0, jsize val1)
  { return p->NewString (this, val0, val1); }

  jint GetStringLength (jstring val0)
  { return p->GetStringLength (this, val0); }

  const jchar * GetStringChars (jstring val0, jboolean * val1)
  { return p->GetStringChars (this, val0, val1); }

  void ReleaseStringChars (jstring val0, const jchar * val1)
  { p->ReleaseStringChars (this, val0, val1); }

  jstring NewStringUTF (const char * val0)
  { return p->NewStringUTF (this, val0); }

  jsize GetStringUTFLength (jstring val0)
  { return p->GetStringUTFLength (this, val0); }

  const char * GetStringUTFChars (jstring val0, jboolean * val1)
  { return p->GetStringUTFChars (this, val0, val1); }

  void ReleaseStringUTFChars (jstring val0, const char * val1)
  { p->ReleaseStringUTFChars (this, val0, val1); }

  jsize GetArrayLength (jarray val0)
  { return p->GetArrayLength (this, val0); }

  jarray NewObjectArray (jsize val0, jclass cl1, jobject obj2)
  { return p->NewObjectArray (this, val0, cl1, obj2); }

  jobject GetObjectArrayElement (jobjectArray val0, jsize val1)
  { return p->GetObjectArrayElement (this, val0, val1); }

  void SetObjectArrayElement (jobjectArray val0, jsize val1, jobject obj2)
  { p->SetObjectArrayElement (this, val0, val1, obj2); }

  jbooleanArray NewBooleanArray (jsize val0)
  { return p->NewBooleanArray (this, val0); }

  jbyteArray NewByteArray (jsize val0)
  { return p->NewByteArray (this, val0); }

  jcharArray NewCharArray (jsize val0)
  { return p->NewCharArray (this, val0); }

  jshortArray NewShortArray (jsize val0)
  { return p->NewShortArray (this, val0); }

  jintArray NewIntArray (jsize val0)
  { return p->NewIntArray (this, val0); }

  jlongArray NewLongArray (jsize val0)
  { return p->NewLongArray (this, val0); }

  jfloatArray NewFloatArray (jsize val0)
  { return p->NewFloatArray (this, val0); }

  jdoubleArray NewDoubleArray (jsize val0)
  { return p->NewDoubleArray (this, val0); }

  jboolean * GetBooleanArrayElements (jbooleanArray val0, jboolean * val1)
  { return p->GetBooleanArrayElements (this, val0, val1); }

  jbyte * GetByteArrayElements (jbyteArray val0, jboolean * val1)
  { return p->GetByteArrayElements (this, val0, val1); }

  jchar * GetCharArrayElements (jcharArray val0, jboolean * val1)
  { return p->GetCharArrayElements (this, val0, val1); }

  jshort * GetShortArrayElements (jshortArray val0, jboolean * val1)
  { return p->GetShortArrayElements (this, val0, val1); }

  jint * GetIntArrayElements (jintArray val0, jboolean * val1)
  { return p->GetIntArrayElements (this, val0, val1); }

  jlong * GetLongArrayElements (jlongArray val0, jboolean * val1)
  { return p->GetLongArrayElements (this, val0, val1); }

  jfloat * GetFloatArrayElements (jfloatArray val0, jboolean * val1)
  { return p->GetFloatArrayElements (this, val0, val1); }

  jdouble * GetDoubleArrayElements (jdoubleArray val0, jboolean * val1)
  { return p->GetDoubleArrayElements (this, val0, val1); }

  void ReleaseBooleanArrayElements (jbooleanArray val0, jboolean * val1, jint val2)
  { p->ReleaseBooleanArrayElements (this, val0, val1, val2); }

  void ReleaseByteArrayElements (jbyteArray val0, jbyte * val1, jint val2)
  { p->ReleaseByteArrayElements (this, val0, val1, val2); }

  void ReleaseCharArrayElements (jcharArray val0, jchar * val1, jint val2)
  { p->ReleaseCharArrayElements (this, val0, val1, val2); }

  void ReleaseShortArrayElements (jshortArray val0, jshort * val1, jint val2)
  { p->ReleaseShortArrayElements (this, val0, val1, val2); }

  void ReleaseIntArrayElements (jintArray val0, jint * val1, jint val2)
  { p->ReleaseIntArrayElements (this, val0, val1, val2); }

  void ReleaseLongArrayElements (jlongArray val0, jlong * val1, jint val2)
  { p->ReleaseLongArrayElements (this, val0, val1, val2); }

  void ReleaseFloatArrayElements (jfloatArray val0, jfloat * val1, jint val2)
  { p->ReleaseFloatArrayElements (this, val0, val1, val2); }

  void ReleaseDoubleArrayElements (jdoubleArray val0, jdouble * val1, jint val2)
  { p->ReleaseDoubleArrayElements (this, val0, val1, val2); }

  void GetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void GetByteArrayRegion (jbyteArray val0, jsize val1, jsize val2, jbyte * val3)
  { p->GetByteArrayRegion (this, val0, val1, val2, val3); }

  void GetCharArrayRegion (jcharArray val0, jsize val1, jsize val2, jchar * val3)
  { p->GetCharArrayRegion (this, val0, val1, val2, val3); }

  void GetShortArrayRegion (jshortArray val0, jsize val1, jsize val2, jshort * val3)
  { p->GetShortArrayRegion (this, val0, val1, val2, val3); }

  void GetIntArrayRegion (jintArray val0, jsize val1, jsize val2, jint * val3)
  { p->GetIntArrayRegion (this, val0, val1, val2, val3); }

  void GetLongArrayRegion (jlongArray val0, jsize val1, jsize val2, jlong * val3)
  { p->GetLongArrayRegion (this, val0, val1, val2, val3); }

  void GetFloatArrayRegion (jfloatArray val0, jsize val1, jsize val2, jfloat * val3)
  { p->GetFloatArrayRegion (this, val0, val1, val2, val3); }

  void GetDoubleArrayRegion (jdoubleArray val0, jsize val1, jsize val2, jdouble * val3)
  { p->GetDoubleArrayRegion (this, val0, val1, val2, val3); }

  void SetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void SetByteArrayRegion (jbyteArray val0, jsize val1, jsize val2, jbyte * val3)
  { p->SetByteArrayRegion (this, val0, val1, val2, val3); }

  void SetCharArrayRegion (jcharArray val0, jsize val1, jsize val2, jchar * val3)
  { p->SetCharArrayRegion (this, val0, val1, val2, val3); }

  void SetShortArrayRegion (jshortArray val0, jsize val1, jsize val2, jshort * val3)
  { p->SetShortArrayRegion (this, val0, val1, val2, val3); }

  void SetIntArrayRegion (jintArray val0, jsize val1, jsize val2, jint * val3)
  { p->SetIntArrayRegion (this, val0, val1, val2, val3); }

  void SetLongArrayRegion (jlongArray val0, jsize val1, jsize val2, jlong * val3)
  { p->SetLongArrayRegion (this, val0, val1, val2, val3); }

  void SetFloatArrayRegion (jfloatArray val0, jsize val1, jsize val2, jfloat * val3)
  { p->SetFloatArrayRegion (this, val0, val1, val2, val3); }

  void SetDoubleArrayRegion (jdoubleArray val0, jsize val1, jsize val2, jdouble * val3)
  { p->SetDoubleArrayRegion (this, val0, val1, val2, val3); }

  jint RegisterNatives (jclass cl0, const JNINativeMethod * val1, jint val2)
  { return p->RegisterNatives (this, cl0, val1, val2); }

  jint UnregisterNatives (jclass cl0)
  { return p->UnregisterNatives (this, cl0); }

  jint MonitorEnter (jobject obj0)
  { return p->MonitorEnter (this, obj0); }

  jint MonitorExit (jobject obj0)
  { return p->MonitorExit (this, obj0); }

  jint GetJavaVM (JavaVM ** val0)
  { return p->GetJavaVM (this, val0); }

  void GetStringRegion (jstring val0, jsize val1, jsize val2, jchar * val3)
  { p->GetStringRegion (this, val0, val1, val2, val3); }

  void GetStringUTFRegion (jstring val0, jsize val1, jsize val2, char * val3)
  { p->GetStringUTFRegion (this, val0, val1, val2, val3); }

  void * GetPrimitiveArrayCritical (jarray val0, jboolean * val1)
  { return p->GetPrimitiveArrayCritical (this, val0, val1); }

  void ReleasePrimitiveArrayCritical (jarray val0, void * val1, jint val2)
  { p->ReleasePrimitiveArrayCritical (this, val0, val1, val2); }

  const jchar * GetStringCritical (jstring val0, jboolean * val1)
  { return p->GetStringCritical (this, val0, val1); }

  void ReleaseStringCritical (jstring val0, const jchar * val1)
  { p->ReleaseStringCritical (this, val0, val1); }

  jweak NewWeakGlobalRef (jobject obj0)
  { return p->NewWeakGlobalRef (this, obj0); }

  void DeleteWeakGlobalRef (jweak val0)
  { p->DeleteWeakGlobalRef (this, val0); }

  jboolean ExceptionCheck ()
  { return p->ExceptionCheck (this); }

  jobject NewDirectByteBuffer (void *addr, jlong capacity)
  { return p->NewDirectByteBuffer (this, addr, capacity); }

  void *GetDirectBufferAddress (jobject buf)
  { return p->GetDirectBufferAddress (this, buf); }

  jlong GetDirectBufferCapacity (jobject buf)
  { return p->GetDirectBufferCapacity (this, buf); }
};
#endif /* __cplusplus */

/*
 * Invocation API.
 */

struct JNIInvokeInterface
{
  _Jv_func reserved0;
  _Jv_func reserved1;
  _Jv_func reserved2;

  jint (JNICALL *DestroyJavaVM)         (JavaVM *);
  jint (JNICALL *AttachCurrentThread)   (JavaVM *, void **, void *);
  jint (JNICALL *DetachCurrentThread)   (JavaVM *);
  jint (JNICALL *GetEnv)                (JavaVM *, void **, jint);
  jint (JNICALL *AttachCurrentThreadAsDaemon) (JavaVM *, void **, void *);
};

#ifdef __cplusplus

class _Jv_JavaVM
{
public:
  const struct JNIInvokeInterface *functions;

private:
  /* FIXME: other fields.  */

public:
  jint DestroyJavaVM ()
  { return functions->DestroyJavaVM (this); }

  jint AttachCurrentThread (void **penv, void *args)
  { return functions->AttachCurrentThread (this, penv, args); }

  jint DetachCurrentThread ()
  { return functions->DetachCurrentThread (this); }

  jint GetEnv (void **penv, jint version)
  { return functions->GetEnv (this, penv, version); }

  jint AttachCurrentThreadAsDaemon (void **penv, void *args)
  { return functions->AttachCurrentThreadAsDaemon (this, penv, args); }
};
#endif /* __cplusplus */

typedef struct JavaVMAttachArgs
{
  jint version;			/* Must be JNI_VERSION_1_2.  */
  char *name;			/* The name of the thread (or NULL).  */
  jobject group;		/* Global ref of a ThreadGroup object
				   (or NULL).  */
} JavaVMAttachArgs;

typedef struct JavaVMOption
{
  char *optionString;
  void *extraInfo;
} JavaVMOption;

typedef struct JavaVMInitArgs
{
  /* Must be JNI_VERSION_1_2.  */
  jint version;

  /* Number of options.  */
  jint nOptions;

  /* Options to the VM.  */
  JavaVMOption *options;

  /* Whether we should ignore unrecognized options.  */
  jboolean ignoreUnrecognized;
} JavaVMInitArgs;

#endif /* __GCJ_JNI_H__ */
