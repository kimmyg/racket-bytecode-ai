#lang racket

; redex is the language we want for prototyping
; get the redex book
; make a CESK model in redex and use the given VSHTC redex model as the ground truth
; (by testing on random programs)
; once confident that it is correct, write write an optimized version
; test against CESK model (to identify flaws in the optimized implementation)
; of course, we're not certain that the CESK redex model is correct (just confident), so 
; test against the VSHTC model at the same time. we're looking for CESK redex different from
; VSHTC redex or them the same and optimized CESK different from the others
; if all three are different, all we care about are the redex models, so dismiss the optimized
