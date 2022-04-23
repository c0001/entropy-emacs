;; *** use firefox bookmarks and history query and open
(use-package counsel-ffdata
  ;; disable this package since the `counsel-ffdata-database-path' init with fatal
  :if nil
  :commands (counsel-ffdata-firefox-bookmarks
             counsel-ffdata-firefox-history)

  :eemacs-indhca
  (((:enable t :defer (:data (:adfors (counsel-mode-hook) :adtype hook :pdumper-no-end t)))
    (counsel-mode (counsel counsel-mode-map)))
   ("Counsel Miscellaneous"
    (("C-c c m f b" counsel-ffdata-firefox-bookmarks "Search your Firefox bookmarks"
      :enable t :exit t :eemacs-top-bind t)
     ("C-c c m f h" counsel-ffdata-firefox-history "Search your Firefox history"
      :enable t :exit t :eemacs-top-bind t))))

  :init
  (setq counsel-ffdata-database-path
        (or
         (ignore-errors
           (cl-case system-type
             ((gnu gnu/linux gnu/kfreebsd)
              (expand-file-name
               (car (file-expand-wildcards
                     "~/.mozilla/firefox/*.default-release/places.sqlite"))))
             (windows-nt
              (car (file-expand-wildcards
                    (expand-file-name "Mozilla/Firefox/Profiles/*/places.sqlite"
                                      (getenv "APPDATA")))))))
         ;; FIXME: use fake one to pass package load
         "~/.mozilla/firefox/.default-release/places.sqlite")))