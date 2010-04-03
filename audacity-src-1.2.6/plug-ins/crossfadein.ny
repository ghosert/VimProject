;nyquist plug-in
;version 1
;type process
;name "Cross Fade In"
;action "Cross-Fading In..."
(setq a (diff (const 1) (ramp 1)))
(mult s (diff (const 1) (mult a a)))
