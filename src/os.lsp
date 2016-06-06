;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; os Module
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;;
;;; A module for obtaining information from ths OS for using in newLISP
;;; scripts.
;;;
;;; (c) 2016, Duncan McGreggor

(context 'os)

(define (get-uname flag)
  (->> flag
       (format "uname -%s")
       (exec)
       (first)))

(define (system)
  (get-uname "s"))

(define (machine)
  (get-uname "m"))

(define (version)
  (get-uname "v"))

(define (release)
  (get-uname "r"))

(define (system? names)
  (-> (system)
      (member names)
      (not)
      (not)))

(define (uname)
  (list
    (list "system" (get-system))
    (list "machine" (get-machine))
    (list "version" (get-version))
    (list "release" (get-release))))

(define (platform-check supported)
  (cond
    ((not (system? supported))
      (println
        (format "ERROR: This script doesn't yet support the %s platform."
                (system)))
        (exit))))

(context MAIN)
