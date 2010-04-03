(CLIP-ALG
  (NAME "clip")
  (ARGUMENTS ("sound_type" "s") ("double" "level")) 
  (LINEAR s) ; this will move the scale factor to the result
  (STATE ("double" "lev" "level * s->scale"))
  (START (MIN s))
  (INNER-LOOP "double x = s; output = (sample_type) (x > lev ? lev : (x < -lev ? -lev : x))")
  (TERMINATE (MIN s))
  (LOGICAL-STOP (MIN s))
)
