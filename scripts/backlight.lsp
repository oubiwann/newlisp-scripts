#!/usr/bin/env newlisp

(module "getopts.lsp")

(load "include/const.lsp")
(load "include/clj.lsp")
(load "src/argparse.lsp")
(load "src/os.lsp")

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Constants
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(setq short-desc "An xbacklight wrapper")
(setq version-string
  (format "%s - version %s (%s)" short-desc version release-year))
(setq mac-get-brightness-cmd
  (string "ioreg -c AppleBacklightDisplay -k IODisplayParameters -f -a"
          "|"
          "grep -A 7 '<key>brightness</key>'"
          "|"
          "grep -A 1 '<key>value</key>'"
          "|"
          "tail -1|tr \\\> :|tr \\\< :"
          "|"
          "awk -F: '{print \$3}'"))
(setq mac-inc "key code 113")
(setq mac-dec "key code 107")
(setq mac-update-brightness-cmd
  (string "osascript"
          " -e 'tell application \"System Events\"'"
          " -e '%s'"
          " -e 'end tell'"))
(setq mac-repeat-update-brightness-cmd
  (string "osascript"
          " -e 'tell application \"System Events\" to repeat %d times'"
          " -e '%s'"
          " -e 'end repeat'"))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (script-info script-name)
  (format "%s, %s" script-name version-string))

(define (display-script-info script-name)
  (println (script-info script-name))
  (exit))

(define (display-unsupported-cmd sys cmd)
  (println (format "The '%s' command is not supported on %s." cmd sys))
  (exit))

(define (brightness-linux sys cmd value)
  (case cmd
    ("get" (! "xbacklight -get"))
    ("set" (! (string "xbacklight -set " value)) )
    ("inc" (! (string "xbacklight -inc 10")))
    ("dec" (! (string "xbacklight -dec 10")))
    (true (display-unsupported-cmd sys cmd))))

(define (get-mac-brightness)
  (-> mac-get-brightness-cmd
      (exec)
      (first)
      (integer)
      (div 256)
      (mul 100)))

(define (set-mac-brightness level)
  (! (format mac-repeat-update-brightness-cmd 16 mac-dec))
  (! (format mac-repeat-update-brightness-cmd
             (ceil (mul (div (integer level) 100) 16))
             mac-inc)))

(define (brightness-mac sys cmd value)
  (case cmd
    ("get" (get-mac-brightness))
    ("set" (set-mac-brightness value))
    ("inc" (! (format mac-update-brightness-cmd mac-inc)))
    ("dec" (! (format mac-update-brightness-cmd mac-dec)))
    (true (display-unsupported-cmd sys cmd))))

(define (brightness-cmd cmd value)
  (let ((sys (os:system)))
    (case sys
      ("Linux" (brightness-linux sys cmd value))
      ("Darwin" (brightness-mac sys cmd value))
      (true  (display-unsupported-cmd sys cmd)))))

(define (light-value? value)
  (and (integer? value)
       (>= value 1)
       (<= value 100)))

(define (get-brightness)
  (println (brightness-cmd "get" "")))

(define (set-level value)
  (brightness-cmd "set" value)
  (get-brightness))

(define (increment-brightness)
  (brightness-cmd "inc" 10)
  (get-brightness))

(define (decrement-brightness)
  (brightness-cmd "dec" 10)
  (get-brightness))

(define (display-bad-value level script)
  (print (format "\nERROR: the provided value '%s' is not valid "
                 (string level)))
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

(shortopt "v" (display-script-info (argparse:get-script)) nil "Print version string")
(shortopt "h" (usage (argparse:get-script)) nil "Print this help message")
(longopt "help" (usage (argparse:get-script)) nil "Print this help message")

(new Tree 'parsed)
(parsed (argparse))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main script opts)
  (println)
  (os:platform-check '("Linux" "Darwin"))
  (if (empty? opts)
    (get-brightness)
    (let ((cmd-or-value (first opts)))
      (cond
        ((= cmd-or-value "inc") (increment-brightness))
        ((= cmd-or-value "dec") (decrement-brightness))
        ((light-value? (integer cmd-or-value)) (set-level cmd-or-value))
        ('true (display-bad-value cmd-or-value script)))))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(main (parsed "script")
      (parsed "opts"))
