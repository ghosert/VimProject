;## misc.lsp -- a collection of useful support functions

; enable or disable breaks
(defun bkon () (setq *breakenable* T))
(defun bkoff () (setq *breakenable* NIL))

(bkon)

;; (grindef 'name) - pretty print a function
;;
(defun grindef (e) (pprint (get-lambda-expression (symbol-function e))))

;; (incf <place>), (decf <place>) - add/sub 1 to/from variable
;;
(defmacro incf (symbol) `(setf ,symbol (1+ ,symbol)))
(defmacro decf (symbol) `(setf ,symbol (1- ,symbol)))


;; (push val <place>) - cons val to list
;;
(defmacro push (val lis) `(setf ,lis (cons ,val ,lis)))
(defmacro pop (lis) `(setf ,lis (cdr ,lis)))

;; include this to use RBD's XLISP profiling hooks
;;(load "/afs/andrew/usr/rbd/lib/xlisp/profile.lsp")

;(cond ((boundp 'application-file-name)
;       (load application-file-name)))


(defun get-input-file-name ()
  (let (fname)
    (format t "Input file name: ")
    (setf fname (read-line))
    (cond ((equal fname "") (get-input-file-name))
          (t fname))))


(defun open-output-file ()
  (let (fname)
    (format t "Output file name: ")
    (setf fname (read-line))
    (cond ((equal fname "") t)
          (t (open fname :direction :output)))))


(defmacro while (cond &rest stmts)
  `(prog () loop (if ,cond () (return)) ,@stmts (go loop)))

(defmacro when (test action)
        (list 'cond (list test action)))

; when parens/quotes don't match, try this
; 
(defun file-sexprs ()
  (let ((fin (open (get-input-file-name)))
        inp)
    (while (setf inp (read fin)) (print inp))))

;; get path for currently loading file (if any)
;;
(defun current-path ()
  (let (fullpath n)
    (setf n -1)
    (cond (*loadingfiles*
           (setf fullpath (car *loadingfiles*))
           (dotimes (i (length fullpath))
             (cond ((equal (char fullpath i) *file-separator*)
                    (setf n i))))
           (setf fullpath (subseq fullpath 0 (1+ n)))
           ;; if this is a Mac, use ':' in place of empty path
           (cond ((and (equal fullpath "") 
                       (equal *file-separator* #\:))
                  (setf fullpath ":")))
           fullpath)
          (t nil))))
          
;; real-random -- pick a random real from a range
;;
(defun real-random (from to)
  (cond ((= from to) from)
          (t
         (+ from
           (* (random 10000)
              0.0001
              (- to from))))))

;; power -- raise a number to some power x^y
;;
(defun power (x y)
  (exp (* (log (float x)) y)))
  
