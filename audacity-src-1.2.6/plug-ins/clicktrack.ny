;nyquist plug-in
;version 1
;type generate
;name "Click Track..."
;action "Generating click track..."
;info "Generates a simple click track at a given tempo and time signature"
;control tempo "Tempo" int "beats/minute" 120 30 300
;control sig "Beats per measure" int "beats" 4 1 20
;control measures "Number of measures" int "measures" 32 10 1000
(setf measures (truncate measures))
(setf tempo (truncate tempo))
(setf sig (truncate sig))
(setf ticklen 0.01)
(setf beatlen (/ 60.0 tempo))

; make one measure
(setf measure (stretch-abs ticklen (scale 0.75 (osc 92))))   ;accented
(dotimes (x (- sig 1))
  (setf measure (sim measure
                     (at (* beatlen (+ x 1))                 ;unaccented
                         (stretch-abs ticklen (scale 0.5 (osc 80)))))))
; make the measure exactly the right length
(setf measure (sim measure
                   (stretch-abs (* sig beatlen) (const 0.0))))

; loop measure n times
(setf result measure)
(dotimes (x (- measures 1))
  (setf result (seq result measure)))

; return result
result
