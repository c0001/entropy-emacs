;;; entropy-emacs-ibuffer.el --- entropy-emacs ibuffer configuration  -*- lexical-binding: t; -*-
;;
;; * Copyright (C) 20190607  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-ibuffer.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; #+END_EXAMPLE
;;
;; * Commentary:
;;
;; IBUFFER configuration of `entropy-emacs'.
;;
;; * Configuration:
;;
;; Loading automatically by `entropy-emacs' without hacking warranty.
;;
;; * Code:

;; ** require

(entropy/emacs-defconst/only-allow/local
  __entropy/emacs-ibuffer-current-use-project-stype-p nil)
(entropy/emacs-defvar-local-with-pml
  __entropy/emacs-ibuffer-current-use-project-stype-p/pml nil)
(entropy/emacs-defconst/only-allow/local
  __entropy/emacs-ibuffer-current-project nil)
(entropy/emacs-defvar-local-with-pml
  __entropy/emacs-ibuffer-current-project/pml nil)
(defun entropy/emacs--ibuffer-hook-set-local-vars nil
  (progn
    (setq __entropy/emacs-ibuffer-current-use-project-stype-p/pml
          __entropy/emacs-ibuffer-current-use-project-stype-p)
    (setq __entropy/emacs-ibuffer-current-project/pml
          __entropy/emacs-ibuffer-current-project)))
(defun entropy/emacs--ibuffer-hook-pre-conds nil
  (when (bound-and-true-p nerd-icons-ibuffer-mode)
    (nerd-icons-ibuffer-mode -1)))
(defun entropy/emacs-ibuffer-main ()
  "Call `ibuffer' or `project-ibuffer'.

If `entropy/emacs-enable-ibuffer-project-style' is non-nil then
PRJ-FILTER-MODE is enabled as default

In PRJ-FILTER-MODE With prefix \\[universal-argument], show
`ibuffer-project' for all projects, except for double
\\[universal-argument] which fullback to default mode.
"
  (declare (interactive-only t))
  (interactive)
  (let* ((pref current-prefix-arg)
         (1pref (and pref (equal pref '(4))))
         (prj (and entropy/emacs-enable-ibuffer-project-style
                   (project-current)))
         (prp (and 1pref entropy/emacs-enable-ibuffer-project-style))
         (cprp (and (not pref) prj
                    entropy/emacs-enable-ibuffer-project-style
                    (project-root prj)))
         (__entropy/emacs-ibuffer-current-use-project-stype-p
          (and (or prp cprp) t))
         (__entropy/emacs-ibuffer-current-project (and cprp prj))
         (title
          (if cprp (format "*Ibuffer* - project: %s" cprp)
            (if prp "*Ibuffer* - All projects")))
         (current-prefix-arg nil)
         (inhibit-quit t))
    (if (or prp cprp) (eemacs//ibuffer-project-init-wrapper 'on)
      (eemacs//ibuffer-project-init-wrapper 'off))
    (entropy/emacs-message-simple-progress-message
        (format "Ibuffer invocation: %s" title)
      (eemacs//ibuffer-set-ibuffer-formats)
      (ibuffer
       nil title
       nil nil nil
       (if cprp (eemacs//ibuffer-project-generate-filter-groups/only-current-prj)
         (if prp (ibuffer-project-generate-filter-groups)))))))

;; ** ibuffer core
(use-package ibuffer
  :ensure nil
  :eemacs-tpha
  (((:enable
     t
     :defer
     (:data
      (:adfors
       (entropy/emacs-after-startup-idle-hook)
       :adtype hook :pdumper-no-end t))))
   ("WI&BUF"
    (("C-x C-b" entropy/emacs-ibuffer-main "Begin using Ibuffer to edit a list of buffers"
      :enable t
      :exit t
      :global-bind t))))

  :config

  ;; face spec
  (setq ibuffer-filter-group-name-face
        (list :inherit (list 'font-lock-string-face 'bold)))

  ;; hiden tmp buffers
  (add-hook
   'ibuffer-never-show-predicates
   (entropy/emacs-!cl-defun eemacs//ibuffer-never-show-predicate
       (buff)
     (let ((buffnm (buffer-name buff)))
       (catch :exit
         (dolist
             (el
              (list
               "^ *\\*"    ;special buffers
               (lambda nil
                 (if-let ((prj (or __entropy/emacs-ibuffer-current-project
                                   __entropy/emacs-ibuffer-current-project/pml)))
                     (not (equal prj (project-current nil (eemacs//default-directory buff))))
                   (when (or __entropy/emacs-ibuffer-current-use-project-stype-p
                             __entropy/emacs-ibuffer-current-use-project-stype-p/pml)
                     (not (project-current nil (eemacs//default-directory buff))))))))
           (if (stringp el)
               (and (string-match-p el buffnm) (throw :exit t))
             (and (funcall el) (throw :exit t))))))))

  (entropy/emacs-lazy-load-simple 'counsel
    (progn
      (defun entropy/emacs-ibuffer-find-file ()
        (interactive)
        (let ((default-directory
               (let ((buf (ibuffer-current-buffer)))
                 (if (buffer-live-p buf)
                     (buffer-local-value 'default-directory buf)
                   default-directory))))
          (counsel-find-file default-directory)))
      (define-key ibuffer-mode-map [remap ibuffer-find-file]
                  #'entropy/emacs-ibuffer-find-file)))

  ;; Reduce nervous for redisplay rendering for huge of lines generating
  (advice-patch
   'ibuffer-insert-buffer-line
   '(entropy/emacs-message-simple-progress-message
        "Generating ibuffer line"
      (funcall format buffer mark))
   '(funcall format buffer mark))

  )

;; ** ibuffer all the icons feature
(use-package nerd-icons-ibuffer
  :init
  (entropy/emacs-lazy-initial-advice-before
   '(ibuffer)
   "nerd-icons-ibuffer" "nerd-icons-ibuffer"
   :prompt-type 'prompt-echo
   ;; We must ensure this startup while pdumpre recovery hook since in
   ;; any trail hook the `display-graphic-p' whill return nil while
   ;; pdumper make session and daemon load session.
   :pdumper-no-end nil
   (when (daemonp)
     (entropy/emacs-with-daemon-make-frame-done
       'nerd-icons-ibuffer (&rest _)
       :when-main
       (progn
         (eemacs//ibuffer-set-ibuffer-formats)
         (dolist (buff (buffer-list))
           (with-current-buffer buff
             (when (eq major-mode 'ibuffer-mode)
               (entropy/emacs-message-simple-progress-message
                   (format "eemacs daemon reformat ibuffer '%s'" buff)
                 (entropy/emacs--ibuffer-hook-pre-conds)
                 (ibuffer-update nil t))))))))))

;; ** ibuffer project
(use-package ibuffer-project
  :eemacs-functions (eemacs//ibuffer-project-init-wrapper
                     ibuffer-project-generate-filter-groups)
  :init (setq ibuffer-project-use-cache t)
  :config
  (defun eemacs//ibuffer-project-ibuffer-hook nil
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  (defun eemacs//ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))
  (defun eemacs//ibuffer-project-refine-group-style (on)
    (if (and on (entropy/emacs-icons-displayable-p))
        (progn
          (advice-add #'ibuffer-project-group-name
                      :override #'eemacs//ibuffer-project-group-name)
          (setq ibuffer-project-root-functions
                `((ibuffer-project-project-root
                   .
                   ,(nerd-icons-octicon
                     "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
                  (file-remote-p
                   .
                   ,(nerd-icons-codicon
                     "nf-cod-radio_tower"
                     :height 1.2 :face ibuffer-filter-group-name-face)))))
      (progn
        (advice-remove #'ibuffer-project-group-name #'eemacs//ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              '((ibuffer-project-project-root . "Project")
                (file-remote-p . "Remote"))))))

  (defun eemacs//ibuffer-project-init-wrapper (on/off)
    (cl-case on/off
      (on
       (add-hook 'ibuffer-hook 'eemacs//ibuffer-project-ibuffer-hook)
       (eemacs//ibuffer-project-refine-group-style t))
      (t
       (remove-hook 'ibuffer-hook 'eemacs//ibuffer-project-ibuffer-hook)
       (eemacs//ibuffer-project-refine-group-style nil))))

  (defun eemacs//ibuffer-project-generate-filter-groups/only-current-prj ()
    "Create ibuffer filters based on current project root of buffers."
    (let* ((curprj (project-current))
           (prj-buffers
            (entropy/emacs-mapcar-without-orphans
             (lambda (x)
               (let (prj)
                 (with-current-buffer x
                   (and (setq prj (project-current))
                        (equal prj curprj)
                        x))))
             ;; FIXME: performance issue to grab `project-current' for
             ;; a large length of `buffer-list', we should use
             ;; hashtable cache?
             (buffer-list) nil nil))
           (roots (sort (ibuffer-remove-duplicates
                         (entropy/emacs-mapcar-without-orphans
                          'ibuffer-project-root prj-buffers
                          nil nil))
                        (lambda (a b) (string-lessp (car a) (car b))))))
      (mapcar (lambda (root)
                (cons (ibuffer-project-group-name (car root) (cdr root))
                      `((project-root . ,root))))
              roots)))

  )

;; ** common ibuffer display
(defun eemacs//ibuffer-set-ibuffer-formats nil
  (require 'nerd-icons-ibuffer)
  (entropy/emacs-setf-by-body ibuffer-formats
    `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "")
            ;; Here you may adjust by replacing :right with :center or :left
            ;; According to taste, if you want the icon further from the name
            " "
            ,(if (not (entropy/emacs-icons-displayable-p)) ""
               '(icon 2 2))
            (name 18 18 :left :elide)
            " " (size-h 9 -1 :right)
            " " (mode+ 16 16 :left :elide)
            " " ,(if __entropy/emacs-ibuffer-current-use-project-stype-p
                     'project-file-relative
                   'filename-and-process+))
      (mark " " (name 16 -1) " " filename))))

(defun entropy/emacs-ibuffer--init-common-1 ()
  (let* ((use-prj-filter-p
          __entropy/emacs-ibuffer-current-use-project-stype-p))
    (unless use-prj-filter-p
      ;; NOTE: we don't need to manually update ibuffer since
      ;; `ibuffer-set-filter-groups-by-mode' will internally invoke
      ;; it.
      (entropy/emacs-message-simple-progress-message
          "ibuffer filter with major-modes"
        (ibuffer-set-filter-groups-by-mode)))
    (entropy/emacs--ibuffer-hook-set-local-vars)))

(defun entropy/emacs-ibuffer--init-common ()
  (entropy/emacs-message-simple-progress-message
      "Preparing eemacs ibuffer specs"
    (entropy/emacs--ibuffer-hook-pre-conds)
    (entropy/emacs-ibuffer--init-common-1)))

(add-hook 'ibuffer-hook #'entropy/emacs-ibuffer--init-common
          ;; NOTE: should be at very first
          -100)

;; * provide
(provide 'entropy-emacs-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
