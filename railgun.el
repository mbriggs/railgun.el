;;; railgun.el - be propelled to the right place by the power of magnets

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1


;;; Dependancies: inflections.el


;;; Commentary:

;; The goal of this project is to provide easy ways to get to the places you
;; want to be.

;; railgun-find-libs        - show a list of libs
;; railgun-find-views       - show a list of views
;; railgun-find-controller  - jump to a given controller
;; railgun-find-presenter   - jump to a given presenter
;; railgun-find-helper      - jump to a given helper
;; railgun-find-model       - jump to a given model
;; railgun-find-schema      - find model entry in schema.rb file
;; railgun-find-blueprint   - find the entry in blueprints.rb for a given model (if you use machinist)
;; railgun-find-factory     - find the entry in factories.rb for a given model (if you use factory_girl)

(require 'inflections)
(require 'cl)

(defvar railgun--file-locations-alist
  '((model      . "app/models")
    (controller . "app/controllers")
    (presenter  . "app/presenters")
    (helper     . "app/helpers")
    (domain     . ((path          . "domain")
                   (relative-path . "domain/.*/")
                   (search-path   . (lambda (path)
                                      (directory-files
                                       (railgun-path "domain"))))))
    (lib        . "lib")))

;;; railgun-files

; file is (type path relative-path class)
(defvar railgun--files '())
(defun railgun-files ()
  (if (railgun--files) railgun--files
    (setq railgun--files (build-railgun-files))))

(defun build-railgun-files ()
  (loop with results = '()
        for location in railgun--file-locations-alist
        (let ((type (car location))
              (files (all-files-under-dir-recursively (cdr location))))
          (append results (mapcar (lambda (file)
                                    `(,type
                                      ,file
                                      ,(railgun-relative-path type file)
                                      ,(railgun-class type file)))
                                  files)))))

(defun railgun-relative-path (type path)
  )
(defun railgun-class ())

(defun railgun-path (path)
  (concat (railway-root) path))

(provide 'railgun)
