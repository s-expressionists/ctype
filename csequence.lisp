(in-package #:ctype)

;;;; Representation of the SEQUENCE class.
;;;; No parameters so there's not much to it. Basically exists so that we don't
;;;; need some annoying special cases with cclass of SEQUENCE.

(defmethod ctypep (client (object array) (ct csequence))
  (declare (ignore client))
  t)
(defmethod ctypep (client (object list) (ct csequence))
  (declare (ignore client))
  t)
(defmethod ctypep (client object (ct csequence))
  ;; user extended sequence? allowed on some implementations
  (subclassp client (class-of object) (find-class client 'sequence)))

(defmethod subctypep (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  (values t t))
(defmethod ctype= (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  (values t t))

(defmethod disjointp (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  (values nil t))

(defmethod conjointp (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct csequence))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  ct1)
(defmethod disjoin/2 (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  ct1)
(defmethod subtract (client (ct1 csequence) (ct2 csequence))
  (declare (ignore client))
  (bot))

(defmethod unparse ((ct csequence)) 'sequence)
