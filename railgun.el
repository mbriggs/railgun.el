;;; railgun.el - be propelled to the right place by the power of magnets

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1


;;; Dependancies: inflections.el


;;; Commentary:

;; The goal of this project is to provide easy ways to get to the places you
;; want to be.

;; NOT DONE - railgun-find-views - show a list of views
;; railgun-find-controller  - jump to a given controller
;; railgun-find-presenter   - jump to a given presenter
;; railgun-find-helper      - jump to a given helper
;; railgun-find-model       - jump to a given model
;; railgun-find-schema      - find model entry in schema.rb file
;; railgun-find-blueprint   - find the entry in blueprints.rb for a given model (if you use machinist)
;; railgun-find-factory     - find the entry in factories.rb for a given model (if you use factory_girl)



(require 'inflections)

;; finders

(defun railgun-find-blueprint ()
  (interactive)
  (let* ((root (eproject-root))
         (target (railgun-prompt-for-resource "Blueprint for"))
         (search (concat "^" target ".blueprint")))
    (find-file (concat root "test/blueprints.rb"))
    (or (re-search-forward search nil t)
        (re-search-backward search nil t))))

(defun railgun-find-factory ()
  (interactive)
  (let* ((root (eproject-root))
         (target (railgun-prompt-for-resource "Factory for"))
         (klass (concat "factory.*" target))
         (sym (concat "factory +:" (railgun-table-name-for-model target))))
    (find-file (concat root "spec/factories.rb"))
    (or (or (re-search-forward klass nil t)
            (re-search-backward klass nil t))
        (or (re-search-forward sym nil t)
            (re-search-backward sym nil t)))))

(defun railgun-find-schema ()
  (interactive)
  (let ((name (railgun-table-name-for-model (railgun-prompt-for-resource "Schema of")))
        (root (eproject-root)))

    (cond ((railgun-find-file-if-it-exists "db/schema.rb")
           (railgun-search-in-file (concat "create_table \"" name "\"")))

          ((railgun-find-file-if-it-exists "db/structure.sql")
           (railgun-search-in-file (concat "CREATE TABLE " name " ")))

          (t (message "run rake db:migrate first")))))


(defun railgun-search-in-file (re)
  (or (re-search-forward re nil t)
      (re-search-backward re nil t))
  (message (concat "looking for " name)))

(defun railgun-find-file-if-it-exists (file)
  (let ((path (concat root file)))
    (if (file-exists-p path)
        (find-file path))))

(defun railgun-find-view ()
  (interactive)
  (ido-find-file-in-dir
   (pluralize-string (railgun-view-dir-for-model (railgun-prompt-for-controller "View for")))))

(defun railgun-find-model ()
  (interactive)
  (find-file (railgun-file-name-for-model (railgun-prompt-for-model))))

(defun railgun-find-controller ()
  (interactive)
  (find-file (railgun-file-name-for-controller (railgun-prompt-for-controller))))

(defun railgun-find-presenter ()
  (interactive)
  (find-file (railgun-file-name-for-presenter (railgun-prompt-for-presenter))))

(defun railgun-find-helper ()
  (interactive)
  (find-file (railgun-file-name-for-helper (railgun-prompt-for-helper))))

;;; Prompts

(defun railgun-prompt-for-resource (prompt)
  (let ((model (railgun-class-from-file-name (buffer-file-name))))
    (railgun-prompt prompt (railgun-models) (if (is-railgun-model-p) model))))

(defun railgun-prompt-for-model ()
  (railgun-prompt "Model" (railgun-models)))

(defun railgun-prompt-for-controller (&optional prompt)
  (let ((prompt (or prompt "Controller")))
    (railgun-prompt prompt (railgun-controllers))))

(defun railgun-prompt-for-presenter ()
  (railgun-prompt "Presenter" (railgun-presenters)))

(defun railgun-prompt-for-helper ()
  (railgun-prompt "Helper" (railgun-helpers)))

(defun railgun-prompt (prompt list &optional initial-value)
  (let ((input (ido-completing-read (concat prompt ": ") list nil t initial-value)))
    (if (string= "" input) model input)))

;; resources

(defun railgun-clear-caches ()
  (interactive)
  (setq railgun/models-alist nil)
  (setq railgun/presenters-alist nil)
  (setq railgun/helpers-alist nil)
  (setq railgun/controllers-alist nil))

(defun railgun-model-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/models") ".rb$"))

(defvar railgun/models-alist nil)
(defun railgun-models-alist ()
  (or railgun/models-alist
      (setq railgun/model-alist (mapcar 'railgun-class-and-file-name
                                        (railgun-model-files)))))

(defun railgun-models ()
  (mapcar 'car (railgun-models-alist)))

; controllers

(defun railgun-controller-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/controllers") ".rb$"))

(defvar railgun/controllers-alist nil)
(defun railgun-controllers-alist ()
  (or railgun/controllers-alist
      (setq railgun/controllers-alist (mapcar 'railgun-class-and-file-name
                                        (railgun-controller-files)))))

(defun railgun-controllers ()
  (mapcar 'car (railgun-controllers-alist)))

; presenters

(defun railgun-presenter-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/presenters") ".rb$"))

(defvar railgun/presenters-alist nil)
(defun railgun-presenters-alist ()
  (or railgun/presenters-alist
      (setq railgun/presenters-alist (mapcar 'railgun-class-and-file-name
                                        (railgun-presenter-files)))))

(defun railgun-presenters ()
  (mapcar 'car (railgun-presenters-alist)))

; helpers

(defun railgun-helper-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/helpers") ".rb$"))

(defvar railgun/helpers-alist nil)
(defun railgun-helpers-alist ()
  (or railgun/helpers-alist
      (setq railgun/helpers-alist (mapcar 'railgun-class-and-file-name
                                          (railgun-helper-files)))))

(defun railgun-helpers ()
  (mapcar 'car (railgun-helpers-alist)))

;; parse entities

(defun railgun-table-name-for-model (model)
  (pluralize-string (railgun-table-name-from-file-name
                     (railgun-file-name-for-model model))))

(defun railgun-file-name-for-model (model)
  (cdr (assoc model (railgun-models-alist))))

(defun railgun-file-name-for-controller (controller)
  (cdr (assoc controller (railgun-controllers-alist))))

(defun railgun-file-name-for-presenter (presenter)
  (cdr (assoc presenter (railgun-presenters-alist))))

(defun railgun-file-name-for-helper (helper)
  (cdr (assoc helper (railgun-helpers-alist))))

;; predicates


(defun is-railgun-model-p ()
  (let ((model-regexp (concat "^" (eproject-root) "app/models")))
    (string-match model-regexp (buffer-file-name))))

(defun railgun-model-p (file-name)
  (string-match "app/models" file-name))

(defun railgun-controller-p (file-name)
  (string-match "app/controllers" file-name))

(defun railgun-presenter-p (file-name)
  (string-match "app/presenters" file-name))

(defun railgun-helper-p (file-name)
  (string-match "app/helpers" file-name))

;; parsing


(defun railgun-class-and-file-name (file-name)
  (let ((class (railgun-class-from-file-name file-name)))
    `(,class . ,file-name)))

(defun railgun-class-and-table-name (file-name)
  (let ((class (railgun-class-from-file-name file-name))
        (table (railgun-table-name-from-file-name file-name)))
    `(,class . ,table)))

(defun railgun-table-name-from-file-name (file-name &optional ns)
  "get an underscored version of the current models name, passing in what to use as namespace delimiter"
  (let* ((delim-with (or ns "_"))
         (dir (concat (eproject-root) (railgun-dir-name-for-file-name file-name)))
         (resource (replace-regexp-in-string dir "" file-name))
         (filename (replace-regexp-in-string "/" delim-with resource)))
    (replace-regexp-in-string ".rb$" "" filename)))

(defun railgun-view-dir-for-model (controller)
  (let* ((file-name (railgun-file-name-for-controller controller))
         (dir (railgun-table-name-from-file-name file-name "/"))
         (view-dir (replace-regexp-in-string "_controller$" "" dir)))
    (concat "app/views/" view-dir)))

(defun railgun-dir-name-for-file-name (file-name)
  (cond ((railgun-model-p file-name) "app/models/" )
        ((railgun-controller-p file-name) "app/controllers/" )
        ((railgun-helper-p file-name) "app/helpers/" )
        ((railgun-presenter-p file-name) "app/presenters/" )))

(defun railgun-class-from-file-name (file-name)
  (let* ((table-name (railgun-table-name-from-file-name file-name "::"))
         (capitalized (capitalize table-name)))
    (replace-regexp-in-string "_" "" capitalized)))
(provide 'railgun)
