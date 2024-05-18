#include <stdio.h>

#include "sndfile.h"

#include "portaudio.h"

typedef struct _SNDINFO {
	SNDFILE *sndfile;
	SF_INFO sf_info;
} SNDINFO;

int paProcess(SNDINFO *sndInfo);

void paErrorHandling(PaError paError);

void sfErrorHandling(int errorNumber);

int checkPaError(PaError paError);

int playCallback(void *inputBuffer, void *outputBuffer, unsigned long framesPerBuffer, PaTimestamp outTime, void *userData); 

int main (int argc, char *argv[]) {

	if (argc != 2) {
		printf("You should run this program with one wav file following.");
		return 1;
	}

	SNDFILE *sndfile;
	SF_INFO sf_info;
	sndfile = sf_open(argv[1], SFM_READ, &sf_info);
	if (sndfile == NULL) {
		sfErrorHandling(sf_error(sndfile));
		return 1;
	}

	printf("Open successfully.\n");

	printf("channels: %d\n", sf_info.channels);
	printf("format: %d\n", sf_info.format);
	printf("frames: %d\n", sf_info.frames);
	printf("samplerate: %d\n", sf_info.samplerate);
	printf("sections: %d\n", sf_info.sections);
	printf("seekable: %d\n", sf_info.seekable);

	SNDINFO sndInfo;
	sndInfo.sndfile = sndfile;
	sndInfo.sf_info = sf_info;

	int iProcess = paProcess(&sndInfo);

	int resultCode = 0;
	if (!(resultCode = sf_close(sndfile))) {
		printf("Close Successfully.\n");
	} else {
		sfErrorHandling(resultCode);
		return 1;
	}

	if (iProcess == 1) return 1;

	return 0 ;
}

int paProcess(SNDINFO *sndInfo) {

	printf("port audio initialize.\n");

	PaError paError = Pa_Initialize();

	if (checkPaError(paError)) return 1;

	PortAudioStream *portAudioStream;

	printf("port audio open default stream.\n");

	paError = Pa_OpenDefaultStream(&portAudioStream, 0, sndInfo->sf_info.channels, paFloat32, sndInfo->sf_info.samplerate, 256, 0, playCallback, sndInfo);

	if (checkPaError(paError)) return 1;

	printf("port audio start stream.\n");

	paError = Pa_StartStream(portAudioStream);

	if (checkPaError(paError)) return 1;

	printf("waiting for the wav file playing is finished.\n");
	while (Pa_StreamActive(portAudioStream)) {
	    Pa_Sleep(1000);
	}

	printf("port audio stop stream.\n");

	paError = Pa_StopStream(portAudioStream);

	if (checkPaError(paError)) return 1;

	printf("port audio close stream.\n");

	paError = Pa_CloseStream(portAudioStream);

	if (checkPaError(paError)) return 1;

	printf("port audio terminate.\n");

	paError = Pa_Terminate();

	if (checkPaError(paError)) return 1;

	printf("test play finished.\n");

	return 0;
}

int playCallback(void *inputBuffer, void *outputBuffer, unsigned long framesPerBuffer, PaTimestamp outTime, void *userData) {

	SNDINFO *sndInfo = (SNDINFO *) userData;
	SNDFILE *sndFile = sndInfo->sndfile;
	SF_INFO sf_info = sndInfo->sf_info;

	float *out = (float *) outputBuffer;

	unsigned long readCount = sf_info.channels * framesPerBuffer;
	float ptr[readCount];
	sf_count_t count = sf_read_float(sndFile, ptr, readCount);

	unsigned long i = 0;
	for (; i < count; i++) {
		*out++ = ptr[i];
	}

	if (count < readCount) return 1;
	
	return 0;
}

int checkPaError(PaError paError) {
	if (paError != paNoError) {
		paErrorHandling(paError);
		return 1;
	}
	return 0;
}

void paErrorHandling(PaError paError) {
	printf("There is a error happens when invoking port audio api.");
	printf("Error Number: %d", paError);
	printf("Error Message: %s", Pa_GetErrorText(paError));
	Pa_Terminate();
}

void sfErrorHandling(int errorNumber) {
	printf("There is a error happens when invoking libsndfile api.");
	printf("Error Number: %d", errorNumber);
	printf("Error Message: %s", sf_error_number(errorNumber));
}



