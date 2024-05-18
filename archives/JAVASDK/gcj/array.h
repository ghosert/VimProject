// array.h - Header file for CNI arrays.  -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000, 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_ARRAY_H__
#define __GCJ_ARRAY_H__

#include <java/lang/Object.h>

extern "Java" {

class __JArray : public java::lang::Object
{
protected:
  // This is just a hack to work around a warning emitted by the C++
  // compiler.  We initialize `length' evilly, but it doesn't know
  // that.
  __JArray () : length (0)
  {
  }
public:
  const jsize length;
  friend jsize JvGetArrayLength (__JArray*);
};

template<class T>
class JArray;

template<class T>
inline T* elements(JArray<T>& x);
template<class T>
inline T* elements(JArray<T>* x);

template<class T>
class JArray : public __JArray
{
  T data[0];
public:
  friend T* elements<>(JArray<T>& x);
  friend T* elements<>(JArray<T>* x);
  // T* getData() { return data; }
  // T& operator[](jint i) { return data[i]; }
};

template<class T>
inline T* elements(JArray<T>& x) { return x.data; }
template<class T>
inline T* elements(JArray<T>* x) { return x->data; }

} // end extern "Java"

/* These typesdefs match those in JNI. */
typedef __JArray *jarray;
typedef JArray<jobject> *jobjectArray;
typedef JArray<jboolean> *jbooleanArray;
typedef JArray<jbyte> *jbyteArray;
typedef JArray<jchar> *jcharArray;
typedef JArray<jshort> *jshortArray;
typedef JArray<jint> *jintArray;
typedef JArray<jlong> *jlongArray;
typedef JArray<jfloat> *jfloatArray;
typedef JArray<jdouble> *jdoubleArray;
typedef JArray<jstring> *jstringArray;

extern java::lang::Class _Jv_byteClass, _Jv_shortClass, _Jv_intClass,
  _Jv_longClass, _Jv_booleanClass, _Jv_charClass, _Jv_floatClass,
  _Jv_doubleClass, _Jv_voidClass;
/* The definition of this macro cannot be enclosed in parentheses
   because "JvPrimClass(x)" is used as a template argument.  */
#define JvPrimClass(TYPE) & _Jv_##TYPE##Class

extern "C" jobjectArray _Jv_NewObjectArray(jsize length, jclass, jobject init);
extern "C" jobject _Jv_NewPrimArray (jclass eltype, jint count);

extern inline jobjectArray 
JvNewObjectArray (jsize length, jclass cls, jobject init)
{ 
  return _Jv_NewObjectArray (length, cls, init); 
}

extern inline jcharArray 
JvNewCharArray (jint length)
{
  return (jcharArray) _Jv_NewPrimArray (JvPrimClass (char), length);
}

extern inline jbooleanArray 
JvNewBooleanArray (jint length)
{
  return (jbooleanArray) _Jv_NewPrimArray (JvPrimClass (boolean), length);
}

extern inline jbyteArray 
JvNewByteArray (jint length)
{
  return (jbyteArray) _Jv_NewPrimArray (JvPrimClass (byte), length);
}

extern inline jshortArray 
JvNewShortArray (jint length)
{
  return (jshortArray) _Jv_NewPrimArray (JvPrimClass (short), length);
}

extern inline jintArray 
JvNewIntArray (jint length)
{
  return (jintArray) _Jv_NewPrimArray (JvPrimClass (int), length);
}

extern inline jlongArray 
JvNewLongArray (jint length)
{
  return (jlongArray) _Jv_NewPrimArray (JvPrimClass (long), length);
}

extern inline jfloatArray 
JvNewFloatArray (jint length)
{
  return (jfloatArray) _Jv_NewPrimArray (JvPrimClass (float), length);
}

extern inline jdoubleArray 
JvNewDoubleArray (jint length)
{
  return (jdoubleArray) _Jv_NewPrimArray (JvPrimClass (double), length);
}


extern "C" jstringArray JvConvertArgv(int argc, const char **argv);

inline jsize JvGetArrayLength (jarray array) { return array->length; }

#endif /* __GCJ_ARRAY_H__ */
