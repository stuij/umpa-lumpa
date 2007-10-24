(in-package :cl-user)

(defpackage :umpa-lumpa
  (:nicknames :umpa)
  (:use :common-lisp
        :it.bese.arnesi)
  (:export
   ;; helpers.lisp
   :positive-integer-p
   :non-neg-integer-p
   :pos-int-range
   :word-p
   :non-neg-int-range
   :negate
   :ensure-non-negative
   :trans-to-base
   :nr-to-big-endian-octets
   :nr-to-octets
   :nr-to-big-endian-word-byte-list
   :nr-to-big-endian-halfword-byte-list
   :big-endian-octets-to-nr
   :big-endian-word-byte-list-to-nr
   :big-endian-halfword-byte-list-to-nr
   :pick-power-of-ten
   :right-shift-power-of-ten
   :ror-word
   :rol-word
   :encode-twos-complement
   :aligned
   :align
   :process-output
   :vector-to-list
   :concat-symbol
   :remove-plist
   :run-prog
   :instance-slot-names
   :class-slot-names
   :copy-hash
   :nthcadr
   :fragment))