#!/usr/bin/env newlisp

(module "getopts.lsp")

(load "include/const.lsp")
(load "include/clj.lsp")
(load "include/script.lsp")
(load "src/argparse.lsp")
(load "src/os.lsp")

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Constants
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(setq default-volume-step 5)
(setq short-desc "A command-line volume wrapper")
(setq version-string
  (format "%s - version %s (%s)" short-desc version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (split-line line)
  (parse line " "))

;; Linux vulume functions

(define (parse-limits line)
  (let ((parts (split-line line)))
    (list (list 'min (integer (nth 2 parts)))
          (list 'max (integer (nth 4 parts))))))

(define (parse-vol line)
  (let ((parts (split-line line)))
    (list (list 'volume (integer (nth 3 parts)))
          (list 'percent (integer (first (regex "\\d{1,2}" (nth 4 parts)))))
          (list 'state (first (regex {[^\[\]\%]+} (nth 5 parts)))))))

(define (parse-line line)
  (cond
    ((starts-with line "Limits:")
      (list 'limits (parse-limits line)))
    ((starts-with line "Front Left:")
      (list 'front-left (parse-vol line)))
    ((starts-with line "Front Right:")
      (list 'front-right (parse-vol line)))
    ((starts-with line "Simple mixer control") (list 'name line))
    ((starts-with line "Capabilities:") (list 'capabilities line))
    ((starts-with line "Playback channels:") (list 'playback-channels line))
    ((starts-with line "Mono:") (list 'mono line))
    (true line)))

(define (parse-output output)
  (->> output
       (map trim)
       (map parse-line)))

(define (get-right-volume parsed-data)
  (->> parsed-data
       (assoc 'front-right)
       (last)
       (assoc 'percent)
       (last)))

(define (get-left-volume parsed-data)
  (->> parsed-data
       (assoc 'front-left)
       (last)
       (assoc 'percent)
       (last)))

(define (get-right-volume-state parsed-data)
  (->> parsed-data
       (assoc 'front-left)
       (last)
       (assoc 'state)
       (last)))

(define (get-average-volume parsed-data)
  (/ (+ (get-right-volume parsed-data)
        (get-left-volume parsed-data))
     2))

(define (muted? parsed-data)
  (if (= (get-right-volume-state parsed-data) "off")
    true
    nil))

(define (format-average-volume parsed-data)
  (->> parsed-data
       (get-average-volume)
       (format "%d%%")))

(define (format-balance parsed-data)
  (format "Right: %d%%\nLeft: %d%%"
          (get-right-volume parsed-data)
          (get-left-volume parsed-data)))

(define (format-mute parsed-data)
  (format "Audio output: %s" (get-right-volume-state parsed-data)))

(define (volume-linux sys cmd value)
  (case cmd
    ("get" (exec "amixer -D pulse sget Master"))
    ("set" (exec (format "amixer -D pulse sset Master %s%%" value)))
    ("inc" (exec (format "amixer -D pulse sset Master %d%%+" value)))
    ("dec" (exec (format "amixer -D pulse sset Master %d%%-" value)))
    ("mute" (exec (format "amixer -D pulse sset Master mute")))
    ("unmute" (exec (format "amixer -D pulse sset Master unmute")))
    (true (display-unsupported-cmd cmd))))

(define (volume-cmd cmd value)
  (let ((sys (os:system)))
    (case sys
      ("Linux" (volume-linux sys cmd value))
      (true (display-sys-unsupported-cmd cmd)))))

(define (sound-value? value)
  (and (integer? value)
       (>= value 1)
       (<= value 100)))

(define (get-volume)
  (->> ""
       (volume-cmd "get")
       (parse-output)
       (format-average-volume)))

(define (set-volume value)
  (->> value
       (volume-cmd "set")
       (parse-output)
       (format-average-volume)))

(define (increment-volume)
  (->> default-volume-step
       (volume-cmd "inc")
       (parse-output)
       (format-average-volume)))

(define (get-balance)
  (->> ""
       (volume-cmd "get")
       (parse-output)
       (format-balance)))

(define (decrement-volume)
  (->> default-volume-step
       (volume-cmd "dec")
       (parse-output)
       (format-average-volume)))

(define (toggle-mute)
  (-> (volume-cmd "get" "")
      (parse-output)
      (muted?)
      (if (volume-cmd "unmute" "")
          (volume-cmd "mute" ""))
      (parse-output)
      (format-mute)))

(define (display-bad-value level script)
  (print (format "\nERROR: the provided value '%s' is not valid "
                 (string level)))
  (println "volume level.")
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
    (println "\tThe 'value' parameter will be used to set the volume level.")
    (println "\tAny value between 0 and 100 is valid.")
    (println)
    (println "Commands:")
    (println
      (format cmd-template
              "inc"
              ""
              "Increase volume by 5%"))
    (println
      (format cmd-template
              "dec"
              ""
              "Decrease volume by 5%"))
    (println
      (format cmd-template
              "mute"
              ""
              "Toggle audio output (mute/unmute)"))
    (println
      (format cmd-template
              "bal"
              ""
              "Get the current R/L balance levels"))
    (println
      (format cmd-template
              "<none>"
              ""
              "If no comand is provided, the current volume level is returned"))
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
    (println (get-volume))
    (let ((cmd-or-value (first opts)))
      (cond
        ((= cmd-or-value "inc") (println (increment-volume)))
        ((= cmd-or-value "dec") (println (decrement-volume)))
        ((= cmd-or-value "bal") (println (get-balance)))
        ((= cmd-or-value "mute") (println (toggle-mute)))
        ((sound-value? (integer cmd-or-value))
          (println (set-volume cmd-or-value)))
        ('true (display-bad-value cmd-or-value script)))))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(main (parsed "script")
      (parsed "opts"))
