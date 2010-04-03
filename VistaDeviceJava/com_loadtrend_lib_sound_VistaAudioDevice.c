#include  <jni.h>
#include  <stdio.h>
#include  <windows.h>
#include  "audio-device.h"
#include  "com_loadtrend_lib_sound_VistaAudioDevice.h"
 
static DeviceInfo *pDeviceInfo = NULL;

/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    initializeAudioDevice
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_initializeAudioDevice
  (JNIEnv *env, jobject obj)
{
	 return InitializeAudioDevice(&pDeviceInfo);
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    destroyAudioDevice
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_destroyAudioDevice
  (JNIEnv *env, jobject obj)
{
	return DestroyAudioDevice(pDeviceInfo);
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getCaptureDeviceListCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getCaptureDeviceListCount
  (JNIEnv *env, jobject obj)
{
	return GetCaptureDeviceListCount(pDeviceInfo);
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getCaptureListDeviceName
 * Signature: (II)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getCaptureListDeviceName
  (JNIEnv *env, jobject obj, jint index, jint deviceNameType)
{
	wchar_t szBuffer[MAX_PATH];
	GetCaptureListDeviceName(pDeviceInfo, index, szBuffer, sizeof(szBuffer) / sizeof(szBuffer)[0], deviceNameType);
	return (*env)->NewString(env, szBuffer, wcslen(szBuffer));
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getCaptureListDeviceId
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getCaptureListDeviceId
  (JNIEnv *env, jobject obj, jint index)
{
	wchar_t szBuffer[MAX_PATH];
	GetCaptureListDeviceId(pDeviceInfo, index, szBuffer, sizeof(szBuffer) / sizeof(szBuffer)[0]);
	return (*env)->NewString(env, szBuffer, wcslen(szBuffer));
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getDefaultCaptureDeviceId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getDefaultCaptureDeviceId
  (JNIEnv *env, jobject obj)
{
	wchar_t szBuffer[MAX_PATH];
	GetDefaultCaptureDeviceId(pDeviceInfo, szBuffer, sizeof(szBuffer) / sizeof(szBuffer)[0]);
	return (*env)->NewString(env, szBuffer, wcslen(szBuffer));
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getCaptureListDeviceState
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getCaptureListDeviceState
  (JNIEnv *env, jobject obj, jint index)
{
	return GetCaptureListDeviceState(pDeviceInfo, index);
}

/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    setCaptureListDeviceVolume
 * Signature: (IF)Z
 */
JNIEXPORT jboolean JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_setCaptureListDeviceVolume
  (JNIEnv *env, jobject obj, jint index, jfloat fVolume)
{
	HRESULT hr = SetCaptureListDeviceVolume(pDeviceInfo, index, fVolume);
	if (hr == S_OK) return TRUE;
	return FALSE;
}

/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    getCaptureListDeviceVolume
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_getCaptureListDeviceVolume
  (JNIEnv *env, jobject obj, jint index)
{
	float fVolume = 0.0f;
	HRESULT hr = GetCaptureListDeviceVolume(pDeviceInfo, index, &fVolume);
	if (hr == S_OK) return fVolume;
	return -1.0f;
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    setEndpointVisibility
 * Signature: (Ljava/lang/String;Z)V
 */
JNIEXPORT void JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_setEndpointVisibility
  (JNIEnv *env, jobject obj, jstring deviceId, jboolean bVisibility)
{
	const wchar_t *str;
	str = (*env)->GetStringChars(env, deviceId, NULL);
	SetEndpointVisibility(pDeviceInfo, str, bVisibility);
	(*env)->ReleaseStringChars(env, deviceId, str);
	return;
}


/*
 * Class:     com_loadtrend_lib_sound_VistaAudioDevice
 * Method:    setDefaultEndpoint
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_setDefaultEndpoint
  (JNIEnv *env, jobject obj, jstring deviceId)
{
	const wchar_t *str;
	str = (*env)->GetStringChars(env, deviceId, NULL);
	SetDefaultEndpoint(pDeviceInfo, str);
	(*env)->ReleaseStringChars(env, deviceId, str);
	return;
}

JNIEXPORT jstring JNICALL Java_com_loadtrend_lib_sound_VistaAudioDevice_testString
  (JNIEnv *env, jobject obj, jstring string)
{
	const char *str;
	str = (*env)->GetStringUTFChars(env, string, NULL);
	if (str == NULL) return NULL; /* OutOfMemoryError already trhown */
	printf("Passed in string from Java: %s\n", str);
	(*env)->ReleaseStringUTFChars(env, string, str);
	return string;
}

