#include <stdio.h>
#include <locale.h>
#include <windows.h>
#include "audio-device.h"

int main(int argc, char *argv[]) {

	if (argc != 2) {
		printf("Please input one number after audio-device.exe for selection of audio device.");
		return 1;
	}

	// make sure the console will show the Chinese correctly.
    setlocale(LC_CTYPE, ".936");

    DeviceInfo *pDeviceInfo = NULL;

	BOOL bResult = InitializeAudioDevice(&pDeviceInfo);

	if (!bResult) return 1; // fail to initialize audio device.

	int iCount = GetCaptureDeviceListCount(pDeviceInfo);

    WCHAR szDeviceName[MAX_PATH];  // temp string buffer
    WCHAR szDeviceId[MAX_PATH];  // temp string buffer
    WCHAR szDefaultDeviceId[MAX_PATH];  // temp string buffer

	int defaultIndex = 0;
	GetDefaultCaptureDeviceId(pDeviceInfo, szDefaultDeviceId, sizeof(szDefaultDeviceId) / sizeof(szDefaultDeviceId)[0]);

	int i = 0;
	for (; i < iCount; i++) {
		GetCaptureListDeviceName(pDeviceInfo, i, szDeviceName, sizeof(szDeviceName) / sizeof(szDeviceName)[0], 2);
		GetCaptureListDeviceId(pDeviceInfo, i, szDeviceId, sizeof(szDeviceId) / sizeof(szDeviceId)[0]);
		if (wcscmp(szDeviceId, szDefaultDeviceId) == 0) { // find default index here.
			defaultIndex = i; 
			wprintf(L"Capture device index: %d [default device]\n name: %s\n id: %s\n state: %s\n\n",
					i, szDeviceName, szDeviceId, GetCaptureListDeviceState(pDeviceInfo, i) == TRUE ? L"ENABLED" : L"DISABLED");
		} else {
			wprintf(L"Capture device index: %d\n name: %s\n id: %s\n state: %s\n\n",
					i, szDeviceName, szDeviceId, GetCaptureListDeviceState(pDeviceInfo, i) == TRUE ? L"ENABLED" : L"DISABLED");
		}
	}

	// Get default capture device and show its name.
	GetCaptureListDeviceName(pDeviceInfo, defaultIndex, szDeviceName, sizeof(szDeviceName) / sizeof(szDeviceName)[0], 0);
	wprintf(L"Default device name: %s \n", szDeviceName);

	// Select capture device from argv and show its name.
	GetCaptureListDeviceName(pDeviceInfo, atoi(argv[1]), szDeviceName, sizeof(szDeviceName) / sizeof(szDeviceName)[0], 0);
	wprintf(L"Select device name: %s \n", szDeviceName);

	WCHAR lpstrId[MAX_PATH];
	GetCaptureListDeviceId(pDeviceInfo,  atoi(argv[1]), lpstrId, sizeof(lpstrId) / sizeof(lpstrId)[0]);
	wprintf(L"Select Device ID: %s \n", lpstrId);

	// Get/Set device master volume
	float fVolume = 0;
	GetCaptureListDeviceVolume(pDeviceInfo, atoi(argv[1]), &fVolume);
	wprintf(L"Get Select device master volume: %f \n", fVolume);

	if (fVolume >= 1.0f) {
		fVolume = 0.0f;
	}
	fVolume = fVolume + 0.1f;
	wprintf(L"Set Select device master volume: %f \n", fVolume);
	SetCaptureListDeviceVolume(pDeviceInfo, atoi(argv[1]), fVolume);

	// Get/Set device all channels volume
	UINT iChannelCount = 0;
	GetCaptureListDeviceChannelCount(pDeviceInfo, atoi(argv[1]), &iChannelCount);
	wprintf(L"Get Select device channel count: %d\n", iChannelCount);

	float fChannelVolume = 0;
	wprintf(L"Get Select device channel volumes:");
	UINT j = 0;
	for (; j < iChannelCount; j++) {
	    GetCaptureListDeviceChannelVolume(pDeviceInfo, atoi(argv[1]), &fChannelVolume, j);
	    wprintf(L" %f ", fChannelVolume);
	}
	wprintf(L"\n");

	if (fChannelVolume >= 1.0f) {
	    fChannelVolume = 0.0f;
	}
	fChannelVolume = fChannelVolume + 0.1f; // add 0.1f for the first channel volume.
	SetCaptureListDeviceChannelVolume(pDeviceInfo, atoi(argv[1]), fChannelVolume, 0);
	wprintf(L"Set Select device first channel volume by 0.1 incresed: %f\n", fChannelVolume);

/******************************* Start to show how to hidden AUDIO CORE API ******************************************/

	WAVEFORMATEX *pWaveFormatEx = NULL;
	GetMixFormat(pDeviceInfo, lpstrId, &pWaveFormatEx);
	wprintf(L"Sample per Sec: %d\n", pWaveFormatEx->nSamplesPerSec);


	SetEndpointVisibility(pDeviceInfo, lpstrId, TRUE);
	wprintf(L"SetEndpointVisibility Success on device Id: %s\n", lpstrId);


	SetDefaultEndpoint(pDeviceInfo, lpstrId);
	wprintf(L"SetDefaultEndpoint Success on device id: %s\n", lpstrId);

/******************************* End to show how to hidden AUDIO CORE API ******************************************/

	DestroyAudioDevice(pDeviceInfo);

	return 0;
}

