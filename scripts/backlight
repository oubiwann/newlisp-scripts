#!/usr/bin/env newlisp

(setq prog-name "backlight")
(setq desc "an xbacklight wrapper")
(setq version "1.0.0")
(setq release-year "2016")
(setq version-string
  (format "%s (%s), version %s (%s)" prog-name desc version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (usage script)
  (letn ((base-template "%s %-10s %s")
         (opt-template (append "\t -" base-template)))
    (println)
    (println version-string)
    (println)
    (println
      (format "Usage: %s [options|value]" script))
    (println)
    (println "Options:")
    (dolist
      (o getopts:short)
      (println (format opt-template (o 0) (or (o 1 1) "") (o 1 2))))
    (dolist
      (o getopts:long)
      (println (format opt-template (o 0) (or (o 1 1) "") (o 1 2))))
    (println)
    (print "The 'value' parameter will be used to set the brightness level. ")
    (println "Any value between 1 and 100 is valid.")
    (exit)))

(define (light-value? value)
  (and (integer? value)
       (>= value 1)
       (<= value 100)))

(define (set-level value)
  (! (format "xbacklight -set %s" value)))

(define (display-bad-value level script)
  (print (format "\nERROR: the provided value '%s' is not valid " level))
  (println "brightness level.")
  (cond
    ((= level "0")
      (println)
      (print "INFO: If you want to turn off the display with zero ")
      (println "brightness, please use xbacklight directly.")))
  (usage script))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main script opts)
  (letn ((light-level (first opts))
         (int-level (integer light-level)))
    (cond
      ((light-value? int-level) (set-level light-level))
      ('true (display-bad-value light-level script))))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(module "getopts.lsp")

(setq *args* (main-args))
(setq *script* (nth 1 *args*))
(setq *opts* (2 *args*))

(shortopt "v" (getopts:die version-string) nil "Print version string")
(shortopt "?" (usage *script*) nil "Print this help message")
(shortopt "h" (usage *script*) nil "Print this help message")

(cond
  ((empty? *opts*)
    (println)
    (println "Error: either an option or a value must be provided.")
    (usage *script*)))

(getopts *opts*)

(main *script* *opts*)

