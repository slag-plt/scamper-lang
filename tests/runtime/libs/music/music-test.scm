(import music)

(define wn (dur 4 4))
(define hn (dur 2 4))
(define qn (dur 1 4))
(define en (dur 1 8))
(define sn (dur 1 16))
(define tn (dur 1 32))

(seq
  (par (note "Bb" 4 qn) (note "D" 5 qn) (note "F" 5 qn))
  (par (note "C" 5 qn) (note "E" 5 qn) (note "G" 5 qn))
  (par (note "D" 5 qn) (note "F#" 5 qn) (note "A" 5 qn))
  (par (note "Eb" 5 qn) (note "G" 5 qn) (note "Bb" 5 qn))
  (par (note "F" 5 qn) (note "A" 5 qn) (note "C" 6 qn))
  (par (note "G" 5 qn) (note "B" 5 qn) (note "D" 6 qn))
  (par (note "A" 5 qn) (note "C#" 6 qn) (note "E" 6 qn))
  (par (note "Bb" 5 qn) (note "D" 6 qn) (note "E" 6 qn)))