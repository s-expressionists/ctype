(in-package #:ctype)

(defmethod ctypep (client (object complex) (ct ccomplex))
  (and (ctypep client (realpart object) (ccomplex-ucpt ct))
       (ctypep client (imagpart object) (ccomplex-ucpt ct))))
(defmethod ctypep (client (object t) (ct ccomplex))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 ccomplex) (ct2 ccomplex))
  (subctypep client (ccomplex-ucpt ct1) (ccomplex-ucpt ct2)))

(defmethod ctype= (client (ct1 ccomplex) (ct2 ccomplex))
  (ctype= client (ccomplex-ucpt ct1) (ccomplex-ucpt ct2)))

(defmethod disjointp (client (ct1 ccomplex) (ct2 ccomplex))
  (disjointp client (ccomplex-ucpt ct1) (ccomplex-ucpt ct2)))
(defmethod conjointp (client (ct1 ccomplex) (ct2 ccomplex))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct ccomplex))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 ccomplex) (ct2 ccomplex))
  (let ((conj (conjoin client (ccomplex-ucpt ct1) (ccomplex-ucpt ct2))))
    (if (bot-p conj)
        conj
        (ccomplex conj))))

(defmethod disjoin/2 (client (ct1 ccomplex) (ct2 ccomplex))
  (ccomplex (disjoin client (ccomplex-ucpt ct1) (ccomplex-ucpt ct2))))

(defmethod subtract (client (ct1 ccomplex) (ct2 ccomplex))
  (let ((ucpt (conjoin client (ccomplex-ucpt ct1)
                       (negate client (ccomplex-ucpt ct2)))))
    (if (bot-p ucpt)
        ucpt
        (ccomplex ucpt))))

(defmethod unparse ((ct ccomplex))
  (let ((ucpt (ccomplex-ucpt ct)))
    (if (top-p ucpt)
        'complex
        `(complex ,(unparse ucpt)))))
