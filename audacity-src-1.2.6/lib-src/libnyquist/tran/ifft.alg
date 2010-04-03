(IFFT-ALG
  (NAME "ifft")
  (ARGUMENTS ("time_type" "t0") ("rate_type" "sr")
             ("LVAL" "src") ("long" "stepsize")
             ("LVAL" "window"))
  (SUPPORT-FUNCTIONS "
/* index: index into outbuf whree we get output samples
 * length: size of the frame, window, and outbuf; half size of samples
 * array: spectral frame goes here (why not a local var?)
 * window_len: size of window, should equal length
 * outbuf: real part of samples are multiplied by window and added to
 *          outbuf (after shifting)
 * src: send :NEXT to this object to get next frame
 * stepsize: shift by this many and add each frame
 * samples: result of ifft goes here, real and imag
 * window: multiply samples by window if any 
 *
 * IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either an
 * array of samples or NIL. The output of ifft is simply the
 * concatenation of the samples taken from the array. Later,
 * an ifft will be plugged in and this will return overlapped
 * adds of the ifft's.
 *
 * OVERLAP: stepsize must be less than or equal to the length
 * of real part of the transformed spectrum. A transform step
 * works like this: 
 * (1) shift the output buffer by stepsize samples, filling
 *     the end of the buffer with zeros
 * (2) get and transform an array of spectral coefficients
 * (3) multiply the result by a window
 * (4) add the result to the output buffer
 * (5) output the first stepsize samples of the buffer
 * 
 * DATA FORMAT: the DC component goes in array elem 0
 * Cosine part is in elements 2*i-1
 * Sine part is in elements 2*i
 * Nyquist frequency is in element length-1
 */

#include \"samples.h\"
#include \"fftn.h\"

table_type get_window_samples(LVAL window, sample_type **samples, long *len)
{
    table_type result = NULL;
    if (soundp(window)) {
        sound_type window_sound = getsound(window);
        xlprot1(window); /* maybe not necessary */
        result = sound_to_table(window_sound);
        xlpop();
        *samples = result->samples;
        *len = (long) (result->length + 0.5);
    }
    return result;
}
")

  (SAMPLE-RATE "sr")
  (STATE
          ("long" "index" "stepsize") ; samples index
          ("long" "length" "0")       ; samples length
          ("LVAL" "array" "NULL")
          ("long" "window_len" "0")
          ("sample_type *" "outbuf" "NULL")
          ("LVAL" "src" "src")
          ("long" "stepsize" "stepsize")
          ("sample_type *" "window" "NULL") ; window samples
          ("sample_type *" "samples" "NULL")
          ("table_type" "table" 
           "get_window_samples(window, &susp->window, &susp->window_len)"))
  
  (OUTER-LOOP "
        if (susp->src == NULL) {
out:        togo = 0;   /* indicate termination */
            break;      /* we're done */
        }
        if (susp->index >= susp->stepsize) {
            long i;
            long half_i;
            long n;
            LVAL elem;
            susp->index = 0;
            susp->array = 
                xleval(cons(s_send, cons(susp->src, consa(s_next))));
            if (susp->array == NULL) {
                susp->src = NULL;
                goto out;
            } else if (!vectorp(susp->array)) {
                xlerror(\"array expected\", susp->array);
            } else if (susp->samples == NULL) {
                /* assume arrays are all the same size as first one;
                   now that we know the size, we just have to do this
                   first allocation.
                 */
                susp->length = getsize(susp->array);
                if (susp->length < 1) 
                    xlerror(\"array has no elements\", susp->array);
                if (susp->window && (susp->window_len != susp->length))
                    xlerror(\"window size and spectrum size differ\", 
                            susp->array);
                susp->samples = 
                    (sample_type *) calloc(susp->length * 2,
                                           sizeof(sample_type));
                susp->outbuf = (sample_type *) calloc(susp->length, 
                    sizeof(sample_type));
            } else if (getsize(susp->array) != susp->length) {
                xlerror(\"arrays must all be the same length\", susp->array);
            }

            /* at this point, we have a new array to put samples */
            /* real part will be susp->samples[0:n-1], */
            /* im part in samples[n:2*n-1] */
            n = susp->length;
            elem = getelement(susp->array, 0);
            if (ntype(elem) != FLONUM) {
                xlerror(\"flonum expected\", elem);
            }
            susp->samples[0] = (sample_type) getflonum(elem);
            susp->samples[n] = 0;
            half_i = 0;
            for (i = 1; i < n - 1; i += 2) {
                half_i++;
                elem = getelement(susp->array, i);
                if (ntype(elem) != FLONUM) {
                    xlerror(\"flonum expected\", elem);
                }
                susp->samples[half_i] = susp->samples[n - half_i] = 
                    (sample_type) (getflonum(elem) / 2.0);

                elem = getelement(susp->array, i + 1);
                if (ntype(elem) != FLONUM) {
                    xlerror(\"flonum expected\", elem);
                }
                susp->samples[n + half_i] =
                    -(susp->samples[2*n - half_i] =
                          (sample_type) (getflonum(elem) / 2.0));
            }
            if (n % 2 == 0) {
                elem = getelement(susp->array, n - 1);
                if (ntype(elem) != FLONUM) {
                    xlerror(\"flonum expected\", elem);
                }
                susp->samples[n / 2] = (sample_type) getflonum(elem);
                susp->samples[n + (n / 2)] = 0;
            }
            susp->array = NULL; /* free the array */

            /* here is where the IFFT and windowing should take place */
            fftnf(1, &n, susp->samples, susp->samples + n, -1, 1.0);
            if (susp->window) {
                n = susp->length;
                for (i = 0; i < n; i++) {
                    susp->samples[i] *= susp->window[i];
                }
            }

            /* shift the outbuf */
            n = susp->length - susp->stepsize;
            for (i = 0; i < n; i++) {
                susp->outbuf[i] = susp->outbuf[i + susp->stepsize];
            }

            /* clear end of outbuf */
            for (i = n; i < susp->length; i++) {
                susp->outbuf[i] = 0;
            }

            /* add in the ifft result */
            n = susp->length;
            for (i = 0; i < n; i++) {
                susp->outbuf[i] += susp->samples[i];
            }
/*
            temp_fft = (double *) malloc (susp->length * sizeof(double));
            if (temp_fft == 0) return;
            big_samples = (double *) malloc (susp->length * sizeof(double));
            if (big_samples == 0) return;
            for (i = 0; i < susp->length; i++) {
                big_samples[i] = (double) susp->samples[i];
            }
            rp = rfftw_create_plan(susp->length, FFTW_COMPLEX_TO_REAL, FFTW_ESTIMATE);
            rfftw_one(rp, big_samples, temp_fft);
            rfftw_destroy_plan(rp);
            free(big_samples);
            for (i = 0; i < susp->length; i++) {
                setelement(result, i, cvflonum(temp_fft[i]));
            }
            free (temp_fft);
*/
        }
        togo = MIN(togo, susp->stepsize - susp->index);
")
  (INNER-LOOP "output = outbuf[index++];")
  (CONSTANT "length" "samples" "array" "src" "window")
  (TERMINATE COMPUTED)
  (FINALIZATION "    if (susp->samples) free(susp->samples);
    if (susp->table) table_unref(susp->table);
    if (susp->outbuf) free(susp->outbuf);
")

)

