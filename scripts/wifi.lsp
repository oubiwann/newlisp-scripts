#!/usr/bin/env newlisp

(setq prog-name "Wifi wrapper")
(setq version "1.0.0")
(setq release-year "2016")
(setq version-string
  (format "%s, version %s (%s)" prog-name version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (usage script)
  (letn ((base-template "%s %-18s %s")
         (opt-template (append "\t -" base-template))
         (cmd-template (append "\t " base-template)))
    (println)
    (println version-string)
    (println)
    (println
      (format "Usage: %s [options|command] [command options]" script))
    (println)
    (println "Options:")
    (dolist
      (o getopts:short)
      (println (format opt-template (o 0) (or (o 1 1) "") (o 1 2))))
    (dolist
      (o getopts:long)
      (println (format opt-template (o 0) (or (o 1 1) "") (o 1 2))))
    (println)
    (println "Commands:")
    (println
      (format cmd-template
              "scan"
              ""
              "Display a list of nearby access points"))
    (println
      (format cmd-template
              "join"
              "<SSID> <password>"
              "Join the access point with given password"))
    (exit)))

(define (display-access-points)
  (! "nmcli device wifi list"))

(define (join-access-point opts)
  (let ((ssid (nth 1 opts))
        (password (nth 2 opts)))
    (! (format "nmcli device wifi connect \"%s\" password %s"
               ssid
               password))))

(define (display-unknown-cmd script)
  (println "\nError: Unknown command")
  (usage script))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Entry point
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (main script opts)
  (case (first opts)
    ("scan" (display-access-points))
    ("join" (join-access-point opts))
    (true (display-unknown-cmd script)))
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
    (println "Error: either an option or a command must be provided.")
    (usage *script*)))

(getopts *opts*)

(main *script* *opts*)

