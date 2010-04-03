(ONESHOT-ALG
  (NAME "oneshot")
  (ARGUMENTS ("sound_type" "input") ("double" "level") ("double" "ontime")) 
  (INTERNAL-SCALING input) ; scale factor handled in level
  (STATE ("double" "lev" "level * input->scale")
         ("long" "oncount" "round(ontime * input->sr)")
         ("long" "cnt" "0"))
  (START (MIN input))
  (INNER-LOOP "double x = input;
        if (x > lev) cnt = oncount;
        cnt--;
        output = (cnt >= 0 ? 1.0F : 0.0F);")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
