;nyquist plug-in
;version 1
;type process
;name "Cross Fade Out"
;action "Cross-Fading Out..."
(mult s (diff (const 1) (mult (ramp 1) (ramp 1))))
