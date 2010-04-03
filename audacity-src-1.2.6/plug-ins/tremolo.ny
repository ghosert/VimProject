;nyquist plug-in
;version 1
;type process
;name "Tremolo..."
;action "Applying Tremolo..."
;control amount "Wetness" int "%" 40 0 100
;control lfo "Frequency" real "Hz" 4.0 0.1 10.0
(mult (sum (const (- 1.0 (/ amount 200.0))) (scale (/ amount 200.0) (osc (hz-to-step lfo)))) s)
