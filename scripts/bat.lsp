#!/usr/bin/env newlisp

(module "getopts.lsp")

(load "include/clj.lsp")
(load "src/argparse.lsp")

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Constants
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(setq prog-name "Battery status")
(setq version "1.2.0")
(setq release-year "2016")
(setq version-string
  (format "%s, version %s (%s)" prog-name version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (get-bat-device-path)
  (->> (exec "upower -e")
       (map (lambda (x) (regex ".*BAT.*" x)))
       (clean nil?)
       (first)
       (first)))

(define (get-bat-info bat-path)
  (exec (append "upower -i " bat-path)))

(define (bat-info? line)
  (or (find "state" line)
      (find "to full" line)
      (find "percentage" line)))

(define (get-status)
  (->> (get-bat-device-path)
       (get-bat-info)
       (filter bat-info?)))

(define (usage)
  (println)
  (println version-string)
  (println)
  (argparse:default-usage))

(define (display lines)
  (map println lines))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Set up and parse options
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(shortopt "v" (getopts:die version-string) nil "Print version string")
(shortopt "h" (usage) nil "Print this help message")
(longopt "help" (usage) nil "Print this help message")

(new Tree 'parsed)
(parsed (argparse))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main)
  (println)
  (println (append prog-name ":"))
  (display (get-status))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(main)
