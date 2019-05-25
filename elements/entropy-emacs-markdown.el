;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.2.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Markdown configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; ** require
(require 'entropy-emacs-const)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** main
(use-package markdown-mode
  :commands (markdown-mode)
  :hook (markdown-mode . markdown-toggle-url-hiding)
  :preface
  ;; Render and preview via `grip'
  ;; you can install grip by 'pip install grip'
  (defun markdown-preview-grip ()
    "Render and preview with `grip'."
    (interactive)
    (if (executable-find "grip")
        (let ((port "6419"))
          (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) port)
          (browse-url (format "http://localhost:%s/%s.%s"
                              port
                              (file-name-base)
                              (file-name-extension
                               (buffer-file-name)))))
      (user-error "Please install grip by 'pip install grip'.")))

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-command-map
              ("p" . markdown-preview-grip))
  :config

  ;; Change face for markdown code,pre,inline-code face for using `entropy/emacs-font-face-default'
  (set-face-attribute 'fixed-pitch nil :family entropy/emacs-font-face-default)
  
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  ;; seting export html styl-sheet
  (setq markdown-command-needs-filename t)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (setq markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre[lang] > code').forEach((code) => {
    code.classList.add(code.parentElement.lang);
    hljs.highlightBlock(code);
  });
});
</script>
")

  ;; synchronization previewing
  (use-package markdown-preview-mode
    :commands (markdown-preview-mode)
    :bind (:map markdown-mode-command-map
                ("P" . markdown-preview-mode))
    :config
    (setq markdown-preview-stylesheets
          (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
                "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
                "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
    (setq markdown-preview-javascript
          (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
                "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))
    (defun entropy/emacs-mdp-before-advice (&rest args)
      "Before advice for `markdown-preview-mode' when it trigger
to disable `markdown-preview-mode' for clean up all web-sockets
to prevent ports keeping as causing to next previewing error.

This issue refer to
`https://github.com/ancane/markdown-preview-mode/issues/31'.

For more is force to set locale language coding system to utf-8,
this refer the websocket non-utf-8 cjk chars error."
      (cond
       (markdown-preview-mode
        (markdown-preview-cleanup)
        (message "Clean up all markdown preview websockets done!"))
       (t (entropy/emacs-lang-set-utf-8))))
    (advice-add 'markdown-preview-mode :before #'entropy/emacs-mdp-before-advice)))

(provide 'entropy-emacs-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
