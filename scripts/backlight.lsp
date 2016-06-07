#!/usr/bin/env newlisp

(module "getopts.lsp")

(load "src/argparse.lsp")
(load "src/os.lsp")

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Constants
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(setq prog-name "backlight")
(setq desc "an xbacklight wrapper")
(setq version "1.3.0")
(setq release-year "2016")
(setq version-string
  (format "%s (%s), version %s (%s)" prog-name desc version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (light-value? value)
  (and (integer? value)
       (>= value 1)
       (<= value 100)))

(define (get-brightness)
  (! "xbacklight -get"))

(define (set-level value)
  (! (format "xbacklight -set %s" value))
  (get-brightness))

(define (increment-brightness)
  (! "xbacklight -inc 10")
  (get-brightness))

(define (decrement-brightness)
  (! "xbacklight -dec 10")
  (get-brightness))

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
;;; Set up and parse options
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (usage script)
  (letn ((base-template "%s %-20s\t%s")
         (short-opt-template (append "\t -" base-template))
         (long-opt-template (append "\t--" base-template))
         (cmd-template (append "\t  " base-template)))
    (println)
    (println version-string)
    (println)
    (println
      (format "Usage: %s [options|value]" script))
    (println)
    (println "Options:")
    (dolist
      (o getopts:short)
      (println (format short-opt-template (o 0) "" (o 1 2))))
    (dolist
      (o getopts:long)
      (println (format long-opt-template (o 0) "" (o 1 2))))
    (println)
    (println "Value:")
    (println "\tThe 'value' parameter will be used to set the brightness level.")
    (println "\tAny value between 1 and 100 is valid.")
    (println)
    (println "Commands:")
    (println
      (format cmd-template
              "inc"
              ""
              "Increase brightness by 10"))
    (println
      (format cmd-template
              "dec"
              ""
              "Decrease brightness by 10"))
    (println
      (format cmd-template
              "<none>"
              ""
              "If no comand is provided, the current brightness is returned"))
    (exit)))

(shortopt "v" (getopts:die version-string) nil "Print version string")
(shortopt "h" (usage (argparse:get-script)) nil "Print this help message")
(longopt "help" (usage (argparse:get-script)) nil "Print this help message")

(new Tree 'parsed)
(parsed (argparse))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main script opts)
  (println)
  (os:platform-check '("Linux"))
  (if (empty? opts)
    (get-brightness)
    (let ((cmd-or-value (first opts)))
      (cond
        ((= cmd-or-value "inc") (increment-brightness))
        ((= cmd-or-value "dec") (decrement-brightness))
        ((light-value? (integer cmd-or-value)) (set-level cmd-or-value))
        ('true (display-bad-value light-level script)))))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(main (parsed "script")
      (parsed "opts"))
