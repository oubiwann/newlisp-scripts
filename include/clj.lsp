;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Library functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;;
;;; Note that these thrushing macros were copied from the newLISP forum thread
;;; here:
;;; * http://www.newlispfanclub.alh.net/forum/viewtopic.php?f=16&t=4089&p=20296
;;;
;;; (c) 2012, William James & johu

(context '->>)
(define-macro (->>:->> E form)
  (letex (_func
          (if $args (cons '->> (cons (list '->> E form) $args))
            (list? form) (push E form -1)
            (list form E)))
         _func))

(context '->)
(define-macro (->:-> E form)
  (letex (_func
          (if $args (cons '-> (cons (list '-> E form) $args))
            (list? form) (push E form 1)
            (list form E)))
         _func))

(context MAIN)
