#ifndef AUDIO_DEVICE_H
#define AUDIO_DEVICE_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/* 
 * This .h file is a public .h file should be published with the lib file.
 *
 * */
#ifdef AUDIO_DEVICE_EXPORTS
    #define AUDIO_DEVICE_API __declspec(dllexport)
#else
    #define AUDIO_DEVICE_API __declspec(dllimport)
#endif

typedef void DeviceInfo;

AUDIO_DEVICE_API BOOL InitializeAudioDevice(DeviceInfo **ppDeviceInfo);

AUDIO_DEVICE_API void DestroyAudioDevice(DeviceInfo *pDeviceInfo);

//
// Public method
//   Gets a count of the endpoint rendering or capture
//   devices in the current list of such devices.
//
AUDIO_DEVICE_API int GetCaptureDeviceListCount(DeviceInfo *pDeviceInfo);

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
AUDIO_DEVICE_API void GetCaptureListDeviceName(DeviceInfo *pDeviceInfo, int index, LPWSTR szBuffer, int bufferLen, DWORD dwKeyName);

AUDIO_DEVICE_API void GetCaptureListDeviceId(DeviceInfo *pDeviceInfo, int index, LPWSTR szBuffer, int bufferLen);

AUDIO_DEVICE_API void GetDefaultCaptureDeviceId(DeviceInfo *pDeviceInfo, LPWSTR szBuffer, int bufferLen);

// The return value should be TRUE OR FALSE means enabled or disabled.
AUDIO_DEVICE_API BOOL GetCaptureListDeviceState(DeviceInfo *pDeviceInfo, int index);

// Public method
//   Sets the volume level of the default audio session
//   of the currently selected endpoint rendering device.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT SetCaptureListDeviceVolume(DeviceInfo *pDeviceInfo, int index, float fVolume);

// Public method
//   Gets the volume level of the default audio session
//   of the currently selected endpoint rendering device.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceVolume(DeviceInfo *pDeviceInfo, int index, float *pfVolume);

// Below three apis are design for the channel volume.
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceChannelCount(DeviceInfo *pDeviceInfo, int index, UINT *iCount);

// This method will get the specified channel volume.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT GetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float *pfVolume, UINT iChannel);

// This method will set the specified channel volume.
// float fVolume: the range of the value from 0.0 to 1.0
AUDIO_DEVICE_API HRESULT SetCaptureListDeviceChannelVolume(DeviceInfo *pDeviceInfo, int index, float fVolume, UINT iChannel);

/*************************************** Start Hidden Audio Core Api **************************************/

// Enable or disable a device.
AUDIO_DEVICE_API void SetEndpointVisibility(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId, BOOL bVisibility);

// Set a device as default device.
AUDIO_DEVICE_API void SetDefaultEndpoint(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId);

// Get Device Mix Format.
AUDIO_DEVICE_API void GetMixFormat(DeviceInfo *pDeviceInfo, LPCWSTR lpcwstrDeviceId, WAVEFORMATEX **ppWaveFormatEx);

/*************************************** End Hidden Audio Core Api **************************************/

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* AUDIO_DEVICE_H */
