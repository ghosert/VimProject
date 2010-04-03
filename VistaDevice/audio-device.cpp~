#include <assert.h>
#include <windows.h>
#include <mmdeviceapi.h>
#include <functiondiscoverykeys.h>
#include <endpointvolume.h>
// #include <errors.h> remaining here for AMGetErrorTextW Win32 Api below which has been commented.

#include "audio-internal.h"
#include "audio-device.h"

// TODO: g_EventContext has not been used for audio volume notification purpose.

void _GetCaptureDeviceList(InternalDeviceInfo *pInternalDeviceInfo)
{
	IMMDeviceEnumerator *pEnumerator = pInternalDeviceInfo->pEnumerator;
	IMMDeviceCollection *pCaptureCollection = NULL;
	// DEVICE_STATE_DISABLED | DEVICE_STATE_NOTPRESENT seems not work here.
	// So the google propose using 0x10000000 instead of the tags above.
    HRESULT hr = pEnumerator->EnumAudioEndpoints(eCapture, DEVICE_STATE_ACTIVE | 0x10000000, &pCaptureCollection);

	assert(hr == S_OK);

	SAFE_RELEASE(pInternalDeviceInfo->pCaptureCollection);
	pInternalDeviceInfo->pCaptureCollection = pCaptureCollection;

	return;
}

AUDIO_DEVICE_API BOOL InitializeAudioDevice(DeviceInfo **ppDeviceInfo)
{
	// Allocate the memory to DeviceInfo.
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) malloc(sizeof(InternalDeviceInfo));

	if (pInternalDeviceInfo == NULL) return FALSE;

	// Initialize IMMDeviceEnumerator, and set it to DeviceInfo.
	IMMDeviceEnumerator *pEnumerator = NULL;
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
    CoCreateInstance(__uuidof(MMDeviceEnumerator), NULL,
                     CLSCTX_ALL, __uuidof(IMMDeviceEnumerator),
                     (void**)&pEnumerator);
	pInternalDeviceInfo->pEnumerator = pEnumerator;

	// Set IMMDeviceCollection to DeviceInfo.
	pInternalDeviceInfo->pCaptureCollection = NULL;
	_GetCaptureDeviceList(pInternalDeviceInfo);

	// Initia IPolicyConfig, and set it to DeviceInfo.
	IPolicyConfig *pPolicyConfig = NULL;
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
    CoCreateInstance(__uuidof(PolicyConfig), NULL,
                     CLSCTX_ALL, __uuidof(IPolicyConfig),
                     (void**)&pPolicyConfig);
	pInternalDeviceInfo->pPolicyConfig = pPolicyConfig;

	*ppDeviceInfo = (void *) pInternalDeviceInfo;

	return TRUE;
}

AUDIO_DEVICE_API void DestroyAudioDevice(DeviceInfo *pDeviceInfo)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

	SAFE_RELEASE(pInternalDeviceInfo->pPolicyConfig);

	SAFE_RELEASE(pInternalDeviceInfo->pCaptureCollection);

	SAFE_RELEASE(pInternalDeviceInfo->pEnumerator);

	CoUninitialize();

	free(pInternalDeviceInfo);

	return;
}

//
// Public method
//   Gets a count of the endpoint rendering or capture
//   devices in the current list of such devices.
//
AUDIO_DEVICE_API int GetCaptureDeviceListCount(DeviceInfo *pDeviceInfo)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
    UINT count = 0;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->GetCount(&count);
    }
    assert(hr == S_OK);
    return count;
}

// Private method
// dwKeyName means:
// 0 DeviceDesc (main name)
// 1 DeviceInterface_FriendlyName (interface name)
// 2 Device_FriendlyName (main name + interface name)
void _GetDeviceName(IMMDevice *pDevice, LPWSTR pszBuffer, int bufferLen, DWORD dwKeyName)
{
    static const WCHAR szDefault[] = L"<Device not available>";

    HRESULT hr = E_FAIL;
    IPropertyStore *pProps = NULL;
    PROPVARIANT varName;

    // Initialize container for property value.
    PropVariantInit(&varName);

    assert(pszBuffer != NULL);
    assert(bufferLen > 0);
	assert(dwKeyName == 0 || dwKeyName == 1 || dwKeyName == 2);

    if (pDevice != NULL)
    {
        hr = pDevice->OpenPropertyStore(STGM_READ, &pProps);
        if (hr == S_OK)
        {
			switch (dwKeyName) {
				case 0:
                    hr = pProps->GetValue(PKEY_Device_DeviceDesc, &varName);
					break;
				case 1:
                    hr = pProps->GetValue(PKEY_DeviceInterface_FriendlyName, &varName);
					break;
				case 2:
                    hr = pProps->GetValue(PKEY_Device_FriendlyName, &varName);
					break;
			}
        }
    }

    if (hr == S_OK)
    {
        // Found the device name.
        wcsncpy_s(pszBuffer, bufferLen, varName.pwszVal, _TRUNCATE);
    }
    else
    {
        // Failed to find the device name.
        wcsncpy_s(pszBuffer, bufferLen, szDefault, _TRUNCATE);
    }

    PropVariantClear(&varName);
    SAFE_RELEASE(pProps);

    return;
}

//
// Public method
//   Gets the friendly name of an endpoint rendering or capture
//   device from the current list of such devices. The caller
//   uses an index into the list to identify the device.
//
// dwKeyName means:
// 0 DeviceDesc (main name)
// 1 DeviceInterface_FriendlyName (interface name)
// 2 Device_FriendlyName (main name + interface name)
AUDIO_DEVICE_API void GetCaptureListDeviceName(DeviceInfo *pDeviceInfo, int index, LPWSTR szBuffer, int bufferLen, DWORD dwKeyName)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
    IMMDevice *pDevice = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }

    assert(hr == S_OK);

	_GetDeviceName(pDevice, szBuffer, bufferLen, dwKeyName);
	SAFE_RELEASE(pDevice);

	return;
}

AUDIO_DEVICE_API void GetCaptureListDeviceId(DeviceInfo *pDeviceInfo, int index, LPWSTR szBuffer, int bufferLen)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
    IMMDevice *pDevice = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }

    assert(hr == S_OK);

	// Copy and return the Id String.
	WCHAR *szTempId;
	pDevice->GetId(&szTempId);
    wcsncpy_s(szBuffer, bufferLen, szTempId, _TRUNCATE);

	SAFE_RELEASE(pDevice);

	return;
}

AUDIO_DEVICE_API void GetDefaultCaptureDeviceId(DeviceInfo *pDeviceInfo, LPWSTR szBuffer, int bufferLen)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
    IMMDevice *pDevice = NULL;

	hr = pInternalDeviceInfo->pEnumerator->GetDefaultAudioEndpoint(eCapture, eMultimedia, &pDevice);

    assert(hr == S_OK);

	// Copy and return the Id String.
	WCHAR *szTempId;
	pDevice->GetId(&szTempId);
    wcsncpy_s(szBuffer, bufferLen, szTempId, _TRUNCATE);

	SAFE_RELEASE(pDevice);

	return;
}

// The return value should be TRUE OR FALSE means enabled or disabled.
AUDIO_DEVICE_API BOOL GetCaptureListDeviceState(DeviceInfo *pDeviceInfo, int index)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
    IMMDevice *pDevice = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }

    assert(hr == S_OK);

	DWORD dwState;
	pDevice->GetState(&dwState);

	SAFE_RELEASE(pDevice);

	return dwState == DEVICE_STATE_ACTIVE ? TRUE : FALSE;
}

// IAudioEndpointVolume Interface contains lots of infos: volume, mute, channel ... 

// Public method
//   Sets the volume level of the default audio session
//   of the currently selected endpoint rendering device.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT SetCaptureListDeviceVolume(DeviceInfo *pDeviceInfo, int index, float fVolume)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr;
	IMMDevice *pDevice = NULL;
	IAudioEndpointVolume *pAudioVolume = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }
	if (hr != S_OK) goto errors;

    hr = pDevice->Activate(__uuidof(IAudioEndpointVolume), CLSCTX_ALL, NULL, (void**)&pAudioVolume);
	if (hr != S_OK) goto errors;

    // EventContext is used to identify the action owner when client receive volume change notification.
	// See the details in MSSDK/Samples/MultiMedia/Audio/WinAudio/player.cpp
    hr = pAudioVolume->SetMasterVolumeLevelScalar(fVolume, &g_EventContext); 

errors:
    SAFE_RELEASE(pAudioVolume);
    SAFE_RELEASE(pDevice);
	return hr;
}

// Public method
//   Gets the volume level of the default audio session
//   of the currently selected endpoint rendering device.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceVolume(DeviceInfo *pDeviceInfo, int index, float *pfVolume)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr;
	IMMDevice *pDevice = NULL;
	IAudioEndpointVolume *pAudioVolume = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }
	if (hr != S_OK) goto errors;

    hr = pDevice->Activate(__uuidof(IAudioEndpointVolume), CLSCTX_ALL, NULL, (void**)&pAudioVolume);
	if (hr != S_OK) goto errors;

    hr = pAudioVolume->GetMasterVolumeLevelScalar(pfVolume);

errors:
    SAFE_RELEASE(pAudioVolume);
    SAFE_RELEASE(pDevice);
	return hr;
}

// This method will get the number of channels.
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceChannelCount(DeviceInfo *pDeviceInfo, int index, UINT *iCount)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr;
	IMMDevice *pDevice = NULL;
	IAudioEndpointVolume *pAudioVolume = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }
	if (hr != S_OK) goto errors;

    hr = pDevice->Activate(__uuidof(IAudioEndpointVolume), CLSCTX_ALL, NULL, (void**)&pAudioVolume);
	if (hr != S_OK) goto errors;

    hr = pAudioVolume->GetChannelCount(iCount);

errors:
    SAFE_RELEASE(pAudioVolume);
    SAFE_RELEASE(pDevice);
	return hr;
}

// This method will get the specified channel volume.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float *pfVolume, UINT iChannel)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr;
	IMMDevice *pDevice = NULL;
	IAudioEndpointVolume *pAudioVolume = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }
	if (hr != S_OK) goto errors;

    hr = pDevice->Activate(__uuidof(IAudioEndpointVolume), CLSCTX_ALL, NULL, (void**)&pAudioVolume);
	if (hr != S_OK) goto errors;

    hr = pAudioVolume->GetChannelVolumeLevelScalar(iChannel, pfVolume);

errors:
    SAFE_RELEASE(pAudioVolume);
    SAFE_RELEASE(pDevice);
	return hr;
}

// This method will set the specified channel volume.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT SetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float fVolume, UINT iChannel)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr;
	IMMDevice *pDevice = NULL;
	IAudioEndpointVolume *pAudioVolume = NULL;

    if (pInternalDeviceInfo->pCaptureCollection != NULL) {
        hr = pInternalDeviceInfo->pCaptureCollection->Item(index, &pDevice);
    }
	if (hr != S_OK) goto errors;

    hr = pDevice->Activate(__uuidof(IAudioEndpointVolume), CLSCTX_ALL, NULL, (void**)&pAudioVolume);
	if (hr != S_OK) goto errors;

    hr = pAudioVolume->SetChannelVolumeLevelScalar(iChannel, fVolume, &g_EventContext);

errors:
    SAFE_RELEASE(pAudioVolume);
    SAFE_RELEASE(pDevice);
	return hr;
}


/*************************************** Start Hidden Audio Core Api **************************************/

// Enable or disable a device.
AUDIO_DEVICE_API void SetEndpointVisibility(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId, BOOL bVisibility)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
	hr = pInternalDeviceInfo->pPolicyConfig->SetEndpointVisibility(lpcwstrDeviceId, bVisibility);
    assert(hr == S_OK);
	return;
}

// Set a device as default device.
AUDIO_DEVICE_API void SetDefaultEndpoint(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
	hr = pInternalDeviceInfo->pPolicyConfig->SetDefaultEndpoint(lpcwstrDeviceId, eMultimedia);
    assert(hr == S_OK);
	return;
}

// Get Device Mix Format.
AUDIO_DEVICE_API void GetMixFormat(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId, WAVEFORMATEX **ppWaveFormatEx)
{
	InternalDeviceInfo *pInternalDeviceInfo = (InternalDeviceInfo *) pDeviceInfo;

    HRESULT hr = S_OK;
	hr = pInternalDeviceInfo->pPolicyConfig->GetMixFormat(lpcwstrDeviceId, ppWaveFormatEx);

	/* show errors. for this function, I add errors.h in this file and add Quartz.lib in makefile, you can remove them if it is not necessary in the future.
		wprintf(L"GetMixFormat result code: %d \n", hr);
		WCHAR pbuffer[MAX_ERROR_TEXT_LEN];
		AMGetErrorTextW(hr, pbuffer, MAX_ERROR_TEXT_LEN);
		wprintf(L"GetMixFormat hresult: %s\n", pbuffer);
    */

    assert(hr == S_OK);
	return;
}

/*************************************** End Hidden Audio Core Api **************************************/

