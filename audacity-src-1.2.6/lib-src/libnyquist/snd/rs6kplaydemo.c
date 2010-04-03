/* This code shows (I think) how to play audio on an RS6K. 
   It is not used for anything, but might be a nice reference
   in case anyone wants to support AIX under the snd library.
   -RBD
 */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <UMSBAUDDevice.h>
void main(argc, argv)
int argc;
char *argv[];
{
 UMSBAUDDevice adev;
 UMSAudioDevice_ReturnCode rc;
 Environment *ev;
UMSAudioTypes_Buffer buffer;
 long onesecsize ;
 long read_cnt, transferred_cnt;
 int file;
 long sw;
 long osamples;
 char *obyte_order;
 long lgain;
 long rgain;
 long time;
 long pos;
 int bytespersec ;
 int secondtorecord ;
 long channels ;
 long bits ;
/* set up som enviroment */
ev = somGetGlobalEnvironment();
adev = UMSBAUDDeviceNew();
printf("start record session\n");
rc = UMSAudioDevice_open(adev,ev,"/dev/acpa0","RECORD", 			UMSAudioDevice_BlockingIO);
/* establish audio attributes */
 channels = 2 ;
 bits  = 16 ;
rc = UMSAudioDevice_set_sample_rate(adev,ev,22050,&osamples);
rc = UMSAudioDevice_set_bits_per_sample(adev,ev,bits);
rc = UMSAudioDevice_set_number_of_channels(adev,ev,channels);
rc = UMSAudioDevice_set_audio_format_type(adev,ev,"PCM");
rc = UMSAudioDevice_set_byte_order(adev,ev,"MSB");
rc = UMSAudioDevice_set_number_format(adev,ev,"TWOS COMPLEMENT");
rc = UMSAudioDevice_get_byte_order(adev,ev,&obyte_order);
/* you have to free the string after the query */
if (obyte_order) free(obyte_order);
rc = UMSAudioDevice_set_volume(adev,ev,100);
rc = UMSAudioDevice_set_balance(adev,ev,0);
rc = UMSAudioDevice_set_time_format(adev,ev,UMSAudioTypes_Msecs);
lgain = 100; /*maximum left input gain*/
rgain = 100; /*maimum right input gain*/
rc = UMSAudioDevice_enable_input(adev,ev,"LINE_IN",&lgain,&rgain);
rc = UMSAudioDevice_set_monitor(adev,ev,TRUE);
rc = UMSAudioDevice_initialize(adev,ev);
secondtorecord = 10 ;
bytespersec = (bits / 8) * channels ;
onesecsize = bytespersec * osamples ;
time = 1000 * secondtorecord ;
buffer._length = onesecsize ;
buffer._buffer = malloc(onesecsize);
buffer._maximum = onesecsize ;
file = open("/tmp/testaudio",O_WRONLY | O_CREAT | O_TRUNC,0644);
rc = UMSAudioDevice_start(adev,ev);
/* do the recording */
do
{
rc = UMSAudioDevice_read(adev,ev,&buffer,onesecsize,&sw);
write(file,buffer._buffer, bytespersec * sw);
rc = UMSAudioDevice_get_position(adev,ev,&pos);
}
while(time > pos);
rc = UMSAudioDevice_stop(adev,ev);
rc = UMSAudioDevice_close(adev,ev);
close(file);
/* playback session */
printf("start playback session\n");
rc = UMSAudioDevice_open(adev,ev,"/dev/acpa0","PLAY",				 UMSAudioDevice_BlockingIO);
buffer._length = 0 ;
buffer._maximum = onesecsize ;
lgain = 100; /*maximum left input gain*/
rgain = 100; /*maimum right input gain*/
rc = UMSAudioDevice_enable_output(adev,ev,"LINE_OUT",&lgain,&rgain);
rc = UMSAudioDevice_initialize(adev,ev);
file = open("/tmp/testaudio",O_RDONLY,0);
rc = UMSAudioDevice_start(adev,ev);
while((transferred_cnt = read(file,buffer._buffer,onesecsize)))
{
read_cnt = transferred_cnt / bytespersec;
buffer._length = transferred_cnt ;
rc = UMSAudioDevice_write(adev,ev,&buffer,read_cnt,&sw);
}
rc = UMSAudioDevice_play_remaining_data(adev,ev,TRUE);
UMSAudioDevice_stop(adev,ev);
UMSAudioDevice_close(adev,ev);
_somFree(adev);
}
