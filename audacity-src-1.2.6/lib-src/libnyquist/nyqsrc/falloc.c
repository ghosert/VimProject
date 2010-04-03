/*
 * falloc.c
 * data for fugue memory allocation.
 */

#include <stdio.h>
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"

/* special free lists */
CQUE *sample_block_free = NULL;    /* really a sample_block_type */

/* special counts */
int sample_block_used = 0;
int sample_block_low_water = 0;
int sample_block_total = 0;
int snd_list_used = 0;
int sound_used = 0;

/* generic free lists */
CQUE *generic_free[MAXLISTS];


void falloc_init(void)
{
    int i;
    for (i = 0; i < MAXLISTS; i++) generic_free[i] = NULL;
}


/* memory pool */
char *poolp = NULL;
char *poolend = NULL;
int npools = 0;

void sound_already_free_test(s)
  sound_type s;
{
    sound_type sp;
    for (sp = (sound_type) sound_free; sp; sp = (sound_type) ((CQUE *) sp)->qnext) {
        if (s == sp) {
            stdputstr("SOUND ALREADY FREE!!!");
            fflush(stdout);
            sp = 0; sp->list = 0;   /* trap to debugger */
        }
    }
}


/* new_pool -- allocate a new pool from which mem is allocated */
/**/
void new_pool(void)
{
    poolp = (char *) malloc(MAXPOOLSIZE);
    if (poolp == NULL) {
        fprintf(stderr, "fugue: out of memory!\n");
        EXIT(1);
    }
    poolend = poolp + MAXPOOLSIZE;
    npools++;
    /* stick to double word boundaries */
    poolp = (char *) round_size(((long) poolp));
}


/* find_sample_block -- get sample block when freelist is empty */
/* Try these strategies in order:
   1) try free list
   2) use pool to get sample_blocks_low_water + BLOCKS_PER_GC blocks or until
      pool runs out
   3) GC and try free list again, set sample_blocks_low_water to 
      sample_blocks_used
   4) try pool again
   5) allocate new pool and use it
 */
sample_block_type find_sample_block(void)
{	
    sample_block_type sp;
    if (sample_block_total < sample_block_low_water + BLOCKS_PER_GC &&
        check_pool(round_size(sizeof(sample_block_node)))) {
        if (DEBUG_MEM) poolp += DEBUG_MEM_INFO_SIZE;
        sp = (sample_block_type) poolp;
        poolp += round_size(sizeof(sample_block_node));
        sample_block_total++;
/*	printf("fp%d ", sample_block_total - sample_block_low_water); */
    } else {
/*	printf("falloc calling gc\n"); */
        gc();
        sample_block_low_water = sample_block_used;
        if (!Qempty(sample_block_free)) {
            Qget(sample_block_free, sample_block_type, sp);
/*	    printf("gc, then from freelist\n"); */
        } else if (check_pool(round_size(sizeof(sample_block_node)))) {
            if (DEBUG_MEM) poolp += DEBUG_MEM_INFO_SIZE;
            sp = (sample_block_type) poolp;
            poolp += sizeof(sample_block_node);
            sample_block_total++;
/*	    printf("gc, then from pool\n"); */
        } else {
            new_pool();
            if (DEBUG_MEM) poolp += DEBUG_MEM_INFO_SIZE;
            sp = (sample_block_type) poolp;
            poolp += round_size(sizeof(sample_block_node));
            sample_block_total++;
/*	    printf("gc, then new pool\n"); */
        }
    }
    return sp;
}



/* get_from_pool -- return size bytes from pool memory */
/**/
char *get_from_pool(size_t siz)
{
    if (!check_pool(siz)) {
        new_pool();
    }
    poolp += siz;
    if (DEBUG_MEM) poolp += DEBUG_MEM_INFO_SIZE; /* allow for debug info */
    return poolp - siz;
}


