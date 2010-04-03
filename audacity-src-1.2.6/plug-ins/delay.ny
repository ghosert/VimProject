;nyquist plug-in
;version 1
;type process
;name "Delay..."
;action "Performing Delay Effect..."
;info "Demo effect for Nyquist by Roger Dannenberg.\nThis effect creates a fixed number of echos."
;control decay "Decay amount" int "dB" 6 0 24
;control delay "Delay time" real "seconds" 0.5 0.0 5.0
;control count "Number of echos" int "times" 5 1 30

;; Note: this effect will use up memory proportional to
;; delay * count, since that many samples must be buffered
;; before the first block is freed.

(defun delays (s decay delay count)
  (if (= count 0) (cue s)
	 (sim (cue s)
			(loud decay (at delay (delays s decay delay (- count 1)))))))
(stretch-abs 1 (delays s (- 0 decay) delay count))
