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

;; rg-find-libs          - show a list of libs
;; rg-find-views         - show a list of views
;; rg-find-controller    - jump to a given controller
;; rg-find-presenter     - jump to a given presenter
;; rg-find-helper        - jump to a given helper
;; rg-find-model         - jump to a given model
;; rg-find-schema        - find model entry in schema.rb (or structure.sql) file
;; rg-find-blueprint     - find the entry in blueprints.rb for a given model (if you use machinist)
;; rg-find-factory       - find the entry in factories.rb for a given model (if you use factory_girl)
;; rg-create-model       - create a model with a given name (in wide case)
;; rg-create-helper      - create a helper with a given name (in wide case)
;; rg-create-controller  - create a controller with a given name (in wide case)
;; rg-create-spec        - create a spec for the current file
;; rg-create-test        - create a test for the current file

;;; Customizing railgun:

;; you can add another type by adding to rg--class-paths. it is an alist with the format of

;; (type<symbol> . path<string>)

;; the path will determine where we search for files, and how we determine the class. if those are different,
;; you can use the alternate syntax

;; (type<symbol> . (search-path<string> . path-that-doesn't-apply-re<string>))

;; So for example, if you add a "domain" folder in the rails-root, that has a sub-folder which is not
;; part of the class name (real use case)

;; (rg-add-class-path (domain . ("domain/" . "domain/.*/")))
;; (rg-define-finder domain "Entity")

;; which will take all rb files in domain, and given a path of domain/common/foo/foo_class.rb, will consider
;; that to be Foo::FooClass. It will also create a function rg-find-domain which will have a prompt of
;; "Entity: ".

;; the default case is simpler, for example, a presenters directory

;; (presenter . "app/presenters/")
;; (rg-define-finder presenter)

;; to reset the class paths to the default (for example, when switching projects) use rg-reset-class-paths
;; to clear caches (for example, when adding a new file) use rg-clear-caches

(require 'inflections)
(require 'cl)

;;; config

(defvar rg-entity 'model
  "entity type to be used in railgun.
   change this if you have your models as another type")

;;; finders

(defun rg-find-blueprint ()
  (interactive)
  (let* ((class (rg-prompt "Blueprint for" (railgun-entities)))
         (search (concat "^" class ".blueprint")))

    (when (rg-find-file-if-it-exists "test/blueprints.rb")
      (or (re-search-forward search nil t)
        (re-search-backward search nil t)))))

(defun rg-find-factory ()
  (interactive)
  (let* ((file (rg-prompt-for-file "Factory for" (railgun-entities)))
         (class (concat "factory.*" (car file)))
         (sym (concat "factory +:" (rg-table-for-file file))))

    (when (rg-find-file-if-it-exists "spec/factories.rb")
      (or (or (re-search-forward class nil t)
              (re-search-backward class nil t))
          (or (re-search-forward sym nil t)
              (re-search-backward sym nil t))))))

(defun rg-find-schema ()
  (interactive)
  (let ((name (rg-prompt-for-table-name "Schema for")))

    (cond ((rg-find-file-if-it-exists "db/schema.rb")
           (rg-search-in-file (concat "create_table \"" name "\"")))

          ((rg-find-file-if-it-exists "db/structure.sql")
           (rg-search-in-file (concat "CREATE TABLE " name " ")))

          (t (message "can't find schema file, run rake db:migrate first")))))


(defun rg-search-in-file (re)
  (or (re-search-forward re nil t)
      (re-search-backward re nil t))
  (message (concat "looking for " name)))

(defun rg-find-file-if-it-exists (file)
  (let ((path (rg-path file)))
    (if (file-exists-p path)
        (find-file path))))

(defun rg-find-entity ()
  (interactive)
  (let ((prompt (capitalize (symbol-name rg-entity)))
        (list (rg-filter-by-type railgun-entity)))
    (find-file (rg-prompt-for-path (concat prompt ": ") list))))

(defun rg-find-class ()
  (interactive)
  (find-file (rg-prompt-for-path "Class: " (railgun-files))))

(defun rg-find-view ()
  (interactive)
  (let* ((input (rg-prompt "View for: " (railgun-filter-by-type 'controller)))
         (path (rg-file-relative-path (railgun-file-for-class input)))
         (controller-name (replace-regexp-in-string "_controller.rb$" "" path))
         (view-dir (rg-path (concat "app/views/" controller-name))))
    (find-file (ido-read-file-name "View: " view-dir))))

;;; tests

(defun rg-spec? ()
  (eq 'spec (rg-file-type (railgun-current-file-info))))

(defun rg-test? ()
  (let ((type (rg-file-type (railgun-current-file-info))))
    (or (eq 'unit-test type)
        (eq 'func-test))))

(defun rg-file-name-postfix (file-name postfix)
  (replace-regexp-in-string "^\\(.+\\)\\.\\([a-z]+\\)$"
                            (concat "\\1" postfix ".\\2")
                            file))

(defun rg-wide-find-spec-or-test ()
  (interactive)
  (let* ((file (rg-file-name-for-path (buffer-file-name)))
         (spec-file (rg-file-name-postfix file "_spec"))
         (test-file (rg-file-name-postfix file "_test")))
    (rg-goto-file-in-list "Spec/Tests: "
                               (append (rg-filter-by-file-name spec-file)
                                       (rg-filter-by-file-name test-file)))))

(defun rg-wide-find-implementation ()
  (interactive)
  (let* ((file (rg-file-name-for-path (buffer-file-name)))
         (impl (replace-regexp-in-string "\\(_test\\|_spec\\)"))
         (files (rg-filter-by-file-name impl)))
    (rg-goto-file-in-list "Implementation: " files)))

(defun rg-goto-file-in-list (prompt list)
  (let ((len (length list))
        (names (mapcar 'car list)))
    (if (= len 0) (message "Could not find spec or test")
      (find-file (rg-file-path
                  (if (= len 1) (car list)
                    (assoc (ido-completing-read prompt names) list)))))))

(defun rg-find-spec ()
  (interactive)
  (let* ((target (concat (rg-current-class) "Spec"))
         (path (rg-file-path (assoc target (railgun-files)))))
    (and path (find-file path))))

(defun rg-find-test ()
  (interactive)
  (let* ((target (concat (rg-current-class) "Test"))
         (path (rg-file-path (assoc target (railgun-files)))))
    (and path (find-file path))))

(defun rg-create-spec ()
  (interactive)
  (let* ((file (rg-current-file-info))
         (search-path (rg-search-path (railgun-file-type file)))
         (spec-path (replace-regexp-in-string "app/\\(assets/\\)?" "" search-path))
         (path (rg-path (concat "spec/" spec-path (railgun-file-relative-path file)))))
    (find-file (replace-regexp-in-string "\.\\([a-z]+\\)$" "_spec.\\1" path))
    (save-buffer)
    (rg-clear-caches)))

(defun rg-build-test-path (file)
  (let ((relative-path (replace-regexp-in-string ".rb$" "_test.rb" (rg-file-relative-path file)))
        (type (rg-file-type file))
        (base "test/"))

    (rg-path (cond ((eq type 'controller)
                         (concat base "functional/" relative-path))
                        ((eq type 'helper)
                         (concat base "unit/helper" relative-path))
                        (t
                         (concat base "unit/" relative-path))))))

(defun rg-create-test ()
  (interactive)
  (let ((path (rg-build-test-path (railgun-current-file-info))))
    (find-file path)
    (save-buffer)
    (rg-clear-caches)))

(defun rg-find-implementation ()
  (interactive)
  (or (let* ((target (replace-regexp-in-string "\\(Spec\\|Test\\)$" "" (rg-current-class)))
             (path (rg-file-path (assoc target (railgun-files)))))
        (and path (find-file path)))

      (rg-wide-find-implementation)))

(defun rg-find-spec-or-test ()
  (interactive)
  (or (or (rg-find-spec)
          (rg-find-test))
      (rg-wide-find-spec-or-test)))

(defun rg-toggle-test-and-implementation ()
  (interactive)
  (let ((type (rg-file-type (railgun-current-file-info))))
    (if (or (eq 'spec type)
            (eq 'unit-test type))
        (rg-find-implementation)
      (rg-find-spec-or-test))))


;;; define-finder

(defmacro rg-define-finder (type &optional prompt)
  (let ((prompt (or prompt (capitalize (symbol-name type)))))
    `(defun ,(intern (concat "rg-find-" (symbol-name type))) ()
       (interactive)
       (let ((prompt (concat ,prompt ": "))
             (list (rg-filter-by-type (quote ,type))))
         (find-file (rg-prompt-for-path prompt list))))))

(defun rg-prompt-for-table-name (prompt)
  (let ((file (rg-prompt-for-file prompt (railgun-entities))))
    (rg-table-for-path (railgun-file-relative-path file))))

(defun rg-prompt-for-path (prompt list)
  (rg-file-path (railgun-prompt-for-file prompt list)))

(defun rg-prompt-for-file (prompt list)
  (assoc (rg-prompt prompt list) list))

(defun rg-find-path-in-list (class-name list)
  (rg-file-path (assoc class-name list)))

(defun rg-prompt-for (type prompt)
  (rg-prompt prompt (railgun-filter-by-type type)))

(defun rg-prompt (prompt list)
  (ido-completing-read prompt (mapcar 'car list) nil t))

;;; creating

(defmacro rg-define-creator (type &optional suffix initial-template-fn)
  (let ((type-name (capitalize (symbol-name type)))
        (name (intern (concat "rg-create-" (symbol-name type))))
        (search-path (rg-search-path type))
        (template (or initial-template-fn (lambda (input)))))
    `(defun ,name ()
       (interactive)
       (let ((input (read-from-minibuffer (concat "Create " ,type-name ": "))))
         (find-file (concat ,search-path "/" input ,suffix))
         (unless (string= (buffer-string) "")
           (funcall ,template input)
           (save-buffer)
           (rg-clear-caches))))))

(defun rg-helper-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name "Helper"))
    (newline)
    (insert "end")))

(defun rg-model-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name " < ActiveRecord::Base"))
    (newline)
    (insert "end")))

(defun rg-controller-template (type-name)
  (let ((name (rg-constantize type-name)))
    (insert (concat "class " name "Controller < ApplicationController"))
    (newline)
    (insert "end")))

;;; parsing

(defvar rg--default-class-paths
  '((model      . "app/models/")
    (controller . "app/controllers/")
    (presenter  . "app/presenters/")
    (helper     . "app/helpers/")
    (service    . ("app/services/" . "app/services/[a-zA-Z-0-9_]+/"))
    (js         . "app/assets/javascripts/")
    (domain     . ("domain/" . "domain/\\([a-zA-Z0-9_]+/\\)?"))
    (lib        . "lib/")
    (unit-test  . ("test/unit/" . "test/unit/\\(helper/\\)?"))
    (func-test  . "test/functional/")
    (spec       . ("spec/" . "spec/\\(domain/[a-zA-Z0-9_]+/\\|[a-zA-Z0-9_]+/\\)"))))

(defvar rg--class-paths (copy-list railgun--default-class-paths))

(defun rg-add-class-path (path)
  (push path rg--class-paths))

(defun rg-reset-class-paths ()
  (interactive)
  (rg-clear-caches)
  (setq rg--class-paths (copy-list railgun--default-class-paths)))

(defun rg-file-types ()
  (mapcar 'car rg--class-paths))

(defun rg-search-path (type)
  (let ((path (cdr (assoc type rg--class-paths))))
    (if (listp path) (car path) path)))

(defun rg-base-regexp (type)
  (let ((path (cdr (assoc type rg--class-paths))))
    (if (listp path) (cdr path) path)))

(defun rg-relative-path (type path)
  (let ((replace (rg-path (railgun-base-regexp type))))
    (replace-regexp-in-string replace "" path)))

(defun rg-class-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "::" chopped))
         (capitalized (capitalize moduled))
         (constant (replace-regexp-in-string "_" "" capitalized)))
    (if (string-match ".Js$" constant)
        (rg-js-ify constant)
      constant)))

(defun rg-js-ify (class-name)
  (let ((class (replace-regexp-in-string ".Js$" "" class-name)))
    (concat "javascript|" class)))

(defun rg-table-for-file (file)
  (rg-table-for-path (railgun-file-relative-path file)))

(defun rg-table-for-path (path)
  (let* ((chopped (replace-regexp-in-string ".rb$" "" path))
         (moduled (replace-regexp-in-string "/" "_" chopped)))
    (pluralize-string moduled)))


;;; rg-files

(defun rg-debug--message-files ()
  (interactive)
  (rg-clear-caches)
  (print (rg-files)))

(defun rg-clear-caches ()
  (interactive)
  (setq rg--files nil))

(defvar rg--files nil)
(defun rg-files ()
  (if rg--files railgun--files
    (setq rg--files (build-railgun-files))))

(defun rg-entities ()
  (rg-filter-by-type railgun-entity))

(defun rg-filter-by-type (type)
  (delq nil
        (mapcar (lambda (file)
                  (and (eq type (rg-file-type file)) file))
                (rg-files))))

(defun rg-filter-by-file-name (file-name)
  (delq nil
        (mapcar (lambda (file)
                  (and (string= file-name (rg-file-name file))
                       file))
                (rg-files))))

(defun build-rg-files ()
  (loop for type in (rg-file-types)
        append (mapcar 'rg-build-file-info
                       (all-files-under-dir-recursively (rg-path (railgun-search-path type))))))

(defun rg-current-file-info ()
  (rg-find-file-for-path (buffer-file-name)))

(defun rg-current-class ()
  (car (rg-current-file-info)))

(defun rg-build-file-info (path)
  (let* ((relative-path (rg-relative-path type path))
         (class-name (rg-class-for-path relative-path))
         (file-name (rg-file-name-for-path path)))
    `(,class-name ,relative-path ,type ,path ,file-name)))

(defun rg-file-name-for-path (path)
  (replace-regexp-in-string "^/\\(.*/\\)*" "" path))

(defun rg-relative-path-for-class (class)
  (rg-file-relative-path (railgun-file-for-class class)))

(defun rg-file-for-class (class)
  (assoc class (rg-files)))

(defun rg-find-file-for-path (path)
  (find-if '(lambda (info)
              (string= path (rg-file-path info)))
           (rg-files)))

(defun rg-file-relative-path (file) (cadr file))
(defun rg-file-type          (file) (caddr file))
(defun rg-file-path          (file) (cadddr file))
(defun rg-file-name          (file) (caddddr file))

;;; utils


(defun caddddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr (cdr (cdr x))))))

(defun rg-path (path)
  (concat (railway-root) path))

(defun rg-constantize (name)
  (replace-regexp-in-string "_" "" (capitalize name)))

;;; defines

(rg-define-creator helper "_helper.rb" 'railgun-helper-template)
(rg-define-creator model ".rb" 'railgun-model-template)
(rg-define-creator controller "_controller.rb" 'railgun-controller-helper)
(rg-define-creator javascript ".js")
(rg-define-creator stylesheet ".css.sass")
(rg-define-creator service "_service.rb")

(rg-define-finder model)
(rg-define-finder controller)
(rg-define-finder presenter)
(rg-define-finder servvice)
(rg-define-finder helper)
(rg-define-finder domain "Entity")
(rg-define-finder lib)

(provide 'railgun)
