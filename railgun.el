;;; railgun.el - be propelled to the right place by the power of magnets

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 2


;;; Dependancies: inflections.el
;                 railway.el

; railway is basically an extraction of the minor mode and project root functions
; from rinari. If you don't want to use it (or already use rinari), fire me an email
; and i will add customization support to allow for any "project root" function to be used.


;;; Commentary:

;; The goal of this project is to provide easy ways to get to the places you
;; want to be. The built in finders are

;; railgun-find-libs        - show a list of libs
;; railgun-find-views       - show a list of views
;; railgun-find-controller  - jump to a given controller
;; railgun-find-presenter   - jump to a given presenter
;; railgun-find-helper      - jump to a given helper
;; railgun-find-model       - jump to a given model
;; railgun-find-schema      - find model entry in schema.rb file
;; railgun-find-blueprint   - find the entry in blueprints.rb for a given model (if you use machinist)
;; railgun-find-factory     - find the entry in factories.rb for a given model (if you use factory_girl)

;;; Customizing railgun:

;; you can add another type by adding to railgun--class-paths. it is an alist with the format of

;; (type<symbol> . path<string>)

;; the path will determine where we search for files, and how we determine the class. if those are different,
;; you can use the alternate syntax

;; (type<symbol> . (search-path<string> . path-that-doesn't-apply-re<string>))

;; So for example, if you add a "domain" folder in the rails-root, that has a sub-folder which is not
;; part of the class name (real use case)

;; (railgun-add-class-path (domain . ("domain/" . "domain/.*/")))
;; (railgun-define-finder domain "Entity")

;; which will take all rb files in domain, and given a path of domain/common/foo/foo_class.rb, will consider
;; that to be Foo::FooClass. It will also create a function railgun-find-domain which will have a prompt of
;; "Entity: ".

;; the default case is simpler, for example, a presenters directory

;; (presenter . "app/presenters/")
;; (railgun-define-finder presenter)

;; to reset the class paths to the default (for example, when switching projects) use railgun-reset-class-paths
;; to clear caches (for example, when adding a new file) use railgun-clear-caches

(require 'inflections)
(require 'cl)

;;; config

(defvar railgun-entity 'domain
  "entity type to be used in railgun.
   change this if you have your models as another type")

;;; finders

(defun railgun-find-blueprint ()
  (interactive)
  (let* ((class (railgun-prompt "Blueprint for" (railgun-entities)))
         (search (concat "^" class ".blueprint")))

    (when (railgun-find-file-if-it-exists "test/blueprints.rb")
      (or (re-search-forward search nil t)
        (re-search-backward search nil t)))))

(defun railgun-find-factory ()
  (interactive)
  (let* ((file (railgun-prompt-for-file "Factory for" (railgun-entities)))
         (class (concat "factory.*" (car file)))
         (sym (concat "factory +:" (railgun-table-for-file file))))

    (when (railgun-find-file-if-it-exists "spec/factories.rb")
      (or (or (re-search-forward class nil t)
              (re-search-backward class nil t))
          (or (re-search-forward sym nil t)
              (re-search-backward sym nil t))))))

(defun railgun-find-schema ()
  (interactive)
  (let ((name (railgun-prompt-for-table-name "Schema for")))

    (cond ((railgun-find-file-if-it-exists "db/schema.rb")
           (railgun-search-in-file (concat "create_table \"" name "\"")))

          ((railgun-find-file-if-it-exists "db/structure.sql")
           (railgun-search-in-file (concat "CREATE TABLE " name " ")))

          (t (message "can't find schema file, run rake db:migrate first")))))


(defun railgun-search-in-file (re)
  (or (re-search-forward re nil t)
      (re-search-backward re nil t))
  (message (concat "looking for " name)))

(defun railgun-find-file-if-it-exists (file)
  (let ((path (railgun-path file)))
    (if (file-exists-p path)
        (find-file path))))

;;; tests

(defun railgun-find-spec ()
  (interactive)
  (let* ((target (concat (railgun-current-class) "Spec"))
         (path (railgun-file-path (assoc target (railgun-files)))))
    (and path (find-file path))))

(defun railgun-find-test ()
  (interactive)
  (let* ((target (concat (railgun-current-class) "Test"))
         (path (railgun-file-path (assoc target (railgun-files)))))
    (and path (find-file path))))

(defun railgun-find-implementation ()
  (interactive)
  (let* ((target (replace-regexp-in-string "\\(Spec\\|Test\\)$" "" (railgun-current-class)))
         (path (railgun-file-path (assoc target (railgun-files)))))
    (and path (find-file path))))

(defun railgun-find-spec-or-test ()
  (interactive)
  (or (railgun-find-spec)
      (railgun-find-test)
      (message "could not find spec or test for current file")))

(defun railgun-toggle-test-and-implmentation ()
  (interactive)
  (let ((type (railgun-file-type (railgun-current-file-info))))
    (if (or (eq 'spec type)
            (eq 'unit-test type))
        (railgun-find-implementation)
      (railgun-find-spec-or-test))))


;;; define-finder

(defmacro railgun-define-finder (type &optional prompt)
  (let ((prompt (or prompt (capitalize (symbol-name type)))))
    `(defun ,(intern (concat "railgun-find-" (symbol-name type))) ()
       (interactive)
       (let ((prompt (concat ,prompt ": "))
             (list (railgun-filter-by-type (quote ,type))))
         (find-file (railgun-prompt-for-path prompt list))))))

(defun railgun-prompt-for-table-name (prompt)
  (let ((file (railgun-prompt-for-file prompt (railgun-entities))))
    (railgun-table-for-path (railgun-file-relative-path file))))

(defun railgun-prompt-for-path (prompt list)
  (railgun-file-path (railgun-prompt-for-file prompt list)))

(defun railgun-prompt-for-file (prompt list)
  (assoc (railgun-prompt prompt list) list))

(defun railgun-find-path-in-list (class-name list)
  (railgun-file-path (assoc class-name list)))

(defun railgun-prompt-for (type prompt)
  (railgun-prompt prompt (railgun-filter-by-type type)))

(defun railgun-prompt (prompt list)
  (ido-completing-read prompt list nil t))



(railgun-define-finder model)
(railgun-define-finder controller)
(railgun-define-finder presenter)
(railgun-define-finder helper)
(railgun-define-finder domain "Entity")
(railgun-define-finder lib)


;;; parsing

(defvar railgun--default-class-paths
  '((model      . "app/models/")
    (controller . "app/controllers/")
    (presenter  . "app/presenters/")
    (helper     . "app/helpers/")
    (domain     . ("domain/" . "domain/[a-zA-Z0-9_]+/"))
    (lib        . "lib/")
    (unit-test  . "test/unit/")
    (func-test  . "test/functional/")
    (spec       . ("spec/" . "spec/\\(domain/[a-zA-Z0-9_]+/\\|[a-zA-Z0-9_]+/\\)"))))

(defvar railgun--class-paths (copy-list railgun--default-class-paths))

(defun railgun-add-class-path (path)
  (push path railgun--class-paths))

(defun railgun-reset-class-paths ()
  (railgun-clear-caches)
  (setq railgun--class-paths (copy-list railgun--default-class-paths)))

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

(defun railgun-table-for-file (file)
  (railgun-table-for-path (railgun-file-relative-path file)))

(defun railgun-table-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "_" chopped)))
    (pluralize-string moduled)))

;;; railgun-files

(defun railgun-debug--message-files ()
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

(defun railgun-entities ()
  (railgun-filter-by-type railgun-entity))

(defun railgun-filter-by-type (type)
  (delq nil
        (mapcar (lambda (file)
                  (and (eq type (railgun-file-type file)) file))
                (railgun-files))))

(defun build-railgun-files ()
  (loop for type in (railgun-file-types)
        append (mapcar 'railgun-build-file-info
                       (all-files-under-dir-recursively (railgun-search-path type)))))

(defun railgun-current-file-info ()
  (railgun-find-file-for-path (buffer-file-name)))

(defun railgun-current-class ()
  (car (railgun-current-file-info)))

(defun railgun-build-file-info (path)
  (let* ((relative-path (railgun-relative-path type path))
         (class-name (railgun-class-for-path relative-path)))
    `(,class-name ,relative-path ,type ,path)))

(defun railgun-relative-path-for-class (class)
  (railgun-file-relative-path (railgun-file-for-class class)))

(defun railgun-file-for-class (class)
  (assoc class (railgun-files)))

(defun railgun-find-file-for-path (path)
  (find-if '(lambda (info)
              (string= path (railgun-file-path info)))
           (railgun-files)))

(defun railgun-file-relative-path (file) (cadr file))
(defun railgun-file-type          (file) (caddr file))
(defun railgun-file-path          (file) (cadddr file))

;;; utils

(defun railgun-path (path)
  (concat (railway-root) path))

(provide 'railgun)
