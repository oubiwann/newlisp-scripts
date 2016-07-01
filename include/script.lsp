;;;; Common script functions

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Script info
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (script-info script-name)
  (format "%s, %s" script-name version-string))

(define (display-script-info script-name)
  (println (script-info script-name))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Script error functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (display-sys-unsupported-cmd cmd)
  (println
    (format "ERROR: The '%s' command is not supported on %s."
            cmd
            (os:system)))
  (exit))

(define (display-unknown-cmd cmd script)
  (println (format "\nERROR: Unknown command '%s'." cmd))
  (usage script))

(define (display-missing-subarg cmd script)
  (println (format "\nERROR: Command '%s' is missing a required argument."
                   cmd))
  (usage script))

(define (display-unknown-subarg cmd subarg script)
  (println (format "\nERROR: Unknown argument '%s' for command '%s'."
                   subarg cmd))
  (usage script))
