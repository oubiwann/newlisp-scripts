#!/usr/bin/env newlisp

(setq prog-name "Battery status")
(setq version "1.0.0")
(setq release-year "2016")
(setq version-string
  (format "%s, version %s (%s)" prog-name version release-year))

(load "include/clj.lsp")

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
  (getopts:usage))

(define (display lines)
  (map println lines))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main)
  (println)
  (println (append prog-name ":"))
  (display (get-status)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (parse-script-name argv)
  (let (arg1 (first argv))
    (case (length argv)
      (0 "")
      (1 (if (= arg1 "newlisp")
            ""
            arg1))
      (true (if (= arg1 "newlisp")
              (nth 1 argv)
              arg1)))))

(define (parse-opts argv)
  (case (length argv)
    (0 '())
    (1 '())
    (2 (1 argv))
    (true (2 argv))))

(module "getopts.lsp")

(define (parse-args)
  (letn ((argv (main-args))
         (script (parse-script-name argv))
         (opts (parse-opts argv)))
    (getopts opts)
    (list
      (list "args" argv)
      (list "script" script)
      (list "opts" opts))))

(shortopt "v" (getopts:die version-string) nil "Print version string")
(shortopt "?" (usage) nil "Print this help message")
(shortopt "h" (usage) nil "Print this help message")

(new Tree 'parsed)
(parsed (parse-args))

(println "DEBUG:")
(println "Args:" (parsed "args"))
(println "Script: " (parsed "script"))
(println "Opts: " (parsed "opts"))

(main)
(exit))

