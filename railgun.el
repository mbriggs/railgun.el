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

;;; parsing

(defvar railgun--class-paths
  '((model      . "app/models/")
    (controller . "app/controllers/")
    (presenter  . "app/presenters/")
    (helper     . "app/helpers/")
    (domain     . ("domain/" . "domain/.*/"))
    (lib        . "lib/")
    (unit-test  . "test/unit/")
    (func-test  . "test/functional/")
    (spec       . ("spec/" . "spec/\\(domain/.*\\|.*/\\)"))))

(defun railgun-file-types ()
  (mapcar 'car railgun--class-paths))

(defun railgun-search-path (type)
  (let ((path (cdr (assoc type railgun--class-paths))))
    (if (listp path) (car path) path)))

(defun railgun-base-regexp (type)
  (let ((path (cdr (assoc type railgun--class-paths))))
    (if (listp path) (cdr path) path)))

(defun railgun-relative-path (type path)
  (let ((replace (railgun-path (railgun-base-regexp type))))
    (replace-regexp-in-string replace "" path)))

(defun railgun-class-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "::" chopped))
         (capitalized (capitalize moduled)))
    (replace-regexp-in-string "_" "" capitalized)))

(defun railgun-table-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "_" chopped)))
    (pluralize-string moduled)))

;;; railgun-files

(defun railgun-message-files ()
  (interactive)
  (railgun-clear-caches)
  (print (railgun-files)))

(defun railgun-clear-caches ()
  (interactive)
  (setq railgun--files nil))

(defvar railgun--files nil)
(defun railgun-files ()
  (if railgun--files railgun--files
    (setq railgun--files (build-railgun-files))))

(defun railgun-filter-by-type (type)
  (delq nil
        (mapcar (lambda (file)
                  (and (eq type (railgun-file-type file)) file))
                railgun-files)))

;<dlowe> mbriggs: (loop for a in list-a nconc (loop for b in list-b collect
;        b))?
(defun build-railgun-files ()
  (loop for type in (railgun-file-types)
        append (mapcar 'railgun-build-file-info
                       (all-files-under-dir-recursively (railgun-search-path type)))))

(defun railgun-build-file-info (path)
  (let* ((relative-path (railgun-relative-path type path))
         (class-name (railgun-class-for-path relative-path)))
    `(,class-name ,relative-path ,type ,path)))

(defun railgun-file-type (file)
  (cddar file))

(defun railgun-path (path)
  (concat (railway-root) path))

(provide 'railgun)
