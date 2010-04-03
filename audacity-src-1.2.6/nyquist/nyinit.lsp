(expand 5)

(load "xlinit.lsp" :verbose NIL)
(setf *gc-flag* nil)
(load "misc.lsp" :verbose NIL)
(load "evalenv.lsp" :verbose NIL)
(load "printrec.lsp" :verbose NIL)

(load "sndfnint.lsp" :verbose NIL)
(load "seqfnint.lsp" :verbose NIL)

(load "dspprims.lsp" :verbose NIL)
(load "nyquist.lsp" :verbose NIL)
(load "follow.lsp")

(load "system.lsp" :verbose NIL)

(load "seqmidi.lsp" :verbose NIL)
(load "nyqmisc.lsp" :verbose NIL)


;; set to T to get ANSI headers and NIL to get antique headers
(setf *ANSI* NIL)

;; set to T to generate tracing code, NIL to disable tracing code
(setf *WATCH* NIL)

(format t "~%Nyquist -- A Language for Sound Synthesis and Composition~%")
(format t "    Copyright (c) 1991-2003 by Roger B. Dannenberg~%")
(format t "    Version 2.29~%~%")

(setf *gc-flag* t)
