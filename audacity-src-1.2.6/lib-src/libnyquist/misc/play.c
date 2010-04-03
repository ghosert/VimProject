/*

    A cheap command to play sample files to the D/A's.
    The sample files should have no headers. They can be
    eight or sixteen bits, with one or two channels. 
    Sample rate is eigher 44.1 or 22.05.

    NOTE: samples brought from the VAX may have byte order
    reversed. Use "dd conv=swab" to fix this.

    William Walker, University of Illinois, 27 January 1989
    (walker@m.cs.uiuc.edu)
*/

#include <stdio.h>
#include <sound/sound.h>
#define THESIZE 3000000


#include "args.h"

#define	TAG	9839283

main(argc, argv)
char **argv;
{
    int srate = SND_RATE_LOW;
    int nchannels = 1;
    int again = 1;
    int i;
    SNDSoundStruct *cow;
    FILE *data;
    
    ARGLOOP
        FLAGARG('h')	srate = SND_RATE_HIGH;		ENDFLAGARG
        FLAGARG('s')	nchannels = 2;			ENDFLAGARG
        STRINGARG('r')	srate = atoi(p);		ENDSTRINGARG
        FLAGARG('o')	again = 0;			ENDFLAGARG

        BADARG
            fprintf(stderr, "unknown option %c\n", *p);
            goto error;
        ENDBADARG
    ENDARGLOOP

       if (argc != 1) goto error;

  cow = (SNDSoundStruct *) malloc(THESIZE);

  /* these fields should probably be invariant: */
  cow->magic = SND_MAGIC;
  cow->dataLocation = sizeof(SNDSoundStruct);
  cow->dataSize = 0;
  cow->dataFormat = SND_FORMAT_LINEAR_16;
  cow->samplingRate = srate;
  cow->channelCount = nchannels;

  printf("%d Hz sample rate,  %d channels\n",
    cow->samplingRate,
    nchannels);


  do {
    data = fopen(argv[0],"r");
    if (data == NULL) { printf("play: fopen error\n"); exit(1); }
    printf("Read %d bytes\n",cow->dataSize = fread((char *)cow+sizeof(SNDSoundStruct),1,THESIZE-sizeof(SNDSoundStruct),data));
    fclose(data);

      /* printf("Reserve sound for playing (%d)\n",SNDReserve()); */
      printf("Play (%d)\n", SNDStartPlaying(cow,TAG,0,0,(SNDNotificationFun)NULL,(SNDNotificationFun)NULL));
      printf("Wait for played sound (%d)\n",SNDWait(TAG));
      /* printf("Query played sound (%d)\n",SNDQueryPlayedSound()); */
      /* printf("Unreserve sound for playing (%d)\n",SNDUnreserveSoundForPlaying()); */
    if(again) {
        char line[100];
        printf("Again? [y] ");
        gets(line);
        if(line[0] != 'y' && line[0] != 'Y' && line[0] != '\0')
            again = 0;
    }
  } while( again );
  exit(0);
error:
    fprintf(stderr, "\nUsage: play [ -h ] [ -s ] [ -o ] file\n\n");
    fprintf(stderr, "  -h   44.1KHz sample rate (default 22.1KHz)\n");
    fprintf(stderr, "  -s   stereo (default mono)\n");
    fprintf(stderr, "  -o   play only once (default asks to play again)\n");
    fprintf(stderr, "\n");
    exit(1);
}
