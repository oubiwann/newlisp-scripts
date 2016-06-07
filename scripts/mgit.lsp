#!/usr/bin/env newlisp

(module "getopts.lsp")

(load "include/clj.lsp")
(load "src/argparse.lsp")

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Constants
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(setq prog-name "Multi-Git")
(setq version "1.3.0")
(setq release-year "2016")
(setq version-string
  (format "%s, version %s (%s)" prog-name version release-year))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Error functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (display-unknown-cmd cmd script)
  (println (format "\nERROR: Unknown command '%s'." cmd))
  (usage script))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Supporting functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(define (display lines)
  (map (lambda (line) (println (string "\t" line))) lines))

(define (git-cmd path cmd msg)
  (println msg)
  (change-dir path)
  (display (exec (format "git %s" cmd))))

(define (git-branch path)
  (git-cmd
    path
    "branch"
    (format "\nCurrent branch in %s ..." path)))

(define (git-checkout branch path)
  (git-cmd
    path
    (format "checkout %s" branch)
    (format "\nChecking out branch '%s' in %s ..." branch path)))

(define (git-pull path)
  (git-cmd
    path
    "pull"
    (format "\nPulling in %s ..." path)))

(define (git-status path)
  (git-cmd
    path
    "status"
    (format "\nGetting status of %s ..." path)))

(define (git-multi-branch paths)
  (map git-branch paths))

(define (git-multi-checkout branch paths)
  (map (lambda (path) (git-checkout branch path)) paths))

(define (git-multi-pull paths)
  (map git-pull paths))

(define (git-multi-status paths)
  (map git-status paths))

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
      (format "Usage: %s [options|command <path>]" script))
    (println)
    (println "Options:")
    (dolist
      (o getopts:short)
      (println (format short-opt-template (o 0) "" (o 1 2))))
    (dolist
      (o getopts:long)
      (println (format long-opt-template (o 0) "" (o 1 2))))
    (println)
    (println "Commands:")
    (println
      (format cmd-template
              "branch"
              ""
              "Perform 'git branch' in multiple paths"))
    (println
      (format cmd-template
              "checkout"
              "<branch name>"
              "Perform 'git checkout <branch name>' in multiple paths"))
    (println
      (format cmd-template
              "pull"
              ""
              "Perform 'git pull' in multiple paths"))
    (println
      (format cmd-template
              "status"
              ""
              "Perform 'git status' in multiple paths"))
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
  (cond
    ((empty? opts)
      (println)
      (println "ERROR: a command must be provided")
      (usage script))
    ((< (length opts) 2)
      (println)
      (println "ERROR: paths to multiple projects must be provided")
      (usage script)))
  (let ((cmd (first opts))
        (paths (rest opts)))
    (case cmd
      ("branch" (git-multi-branch paths))
      ("checkout" (let ((branch (first paths))
                        (paths (rest paths)))
                    (git-multi-checkout branch paths)))
      ("pull" (git-multi-pull paths))
      ("status" (git-multi-status paths))
      (true (display-unknown-cmd cmd script))))
  (exit))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Run the program
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(main (parsed "script")
      (parsed "opts"))

