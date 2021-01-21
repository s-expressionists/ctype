(in-package #:ctype)

(defmethod ctypep ((object complex) (ct ccomplex))
  (complex-ucptp object (ccomplex-ucpt ct)))
(defmethod ctypep ((object t) (ct ccomplex)) nil)

(defmethod subctypep ((ct1 ccomplex) (ct2 ccomplex))
  (values (equal (ccomplex-ucpt ct1) (ccomplex-ucpt ct2)) t))

(defmethod disjointp ((ct1 ccomplex) (ct2 ccomplex))
  (let ((ucpt1 (ccomplex-ucpt ct1)) (ucpt2 (ccomplex-ucpt ct2)))
    (cond ((eq ucpt1 '*) (values t t))
          ((eq ucpt2 '*) (values t t))
          (t (values (equal ucpt1 ucpt2) t)))))

(defmethod conjoin/2 ((ct1 ccomplex) (ct2 ccomplex))
  (let ((ucpt1 (ccomplex-ucpt ct1)) (ucpt2 (ccomplex-ucpt ct2)))
    (cond ((eq ucpt1 '*) ct2)
          ((eq ucpt2 '*) ct1)
          ((equal ucpt1 ucpt2) ct1)
          (t (bot)))))

(defmethod disjoin/2 ((ct1 ccomplex) (ct2 ccomplex))
  (let ((ucpt1 (ccomplex-ucpt ct1)) (ucpt2 (ccomplex-ucpt ct2)))
    (cond ((eq ucpt1 '*) ct1)
          ((eq ucpt2 '*) ct2)
          ((equal ucpt1 ucpt2) ct1)
          (t (call-next-method)))))

(defmethod subtract ((ct1 ccomplex) (ct2 ccomplex))
  (let ((ucpt1 (ccomplex-ucpt ct1)) (ucpt2 (ccomplex-ucpt ct2)))
    (cond ((eq ucpt2 '*) (bot))
          ((eq ucpt1 '*) (call-next-method))
          ((equal ucpt1 ucpt2) (bot))
          (t ct1))))

(defmethod unparse ((ct ccomplex))
  (let ((ucpt (ccomplex-ucpt ct)))
    (if (eq ucpt '*)
        'complex
        `(complex ,ucpt))))
