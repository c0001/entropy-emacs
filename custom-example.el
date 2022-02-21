 ;;      ___          ___                 ___          ___          ___
 ;;     /\__\        /\  \               /\  \        /\  \        /\  \
 ;;    /:/ _/_       \:\  \       ___   /::\  \      /::\  \      /::\  \   ___
 ;;   /:/ /\__\       \:\  \     /\__\ /:/\:\__\    /:/\:\  \    /:/\:\__\ /|  |
 ;;  /:/ /:/ _/_  _____\:\  \   /:/  //:/ /:/  /   /:/  \:\  \  /:/ /:/  /|:|  |
 ;; /:/_/:/ /\__\/::::::::\__\ /:/__//:/_/:/__/___/:/__/ \:\__\/:/_/:/  / |:|  |
 ;; \:\/:/ /:/  /\:\~~\~~\/__//::\  \\:\/:::::/  /\:\  \ /:/  /\:\/:/  /__|:|__|
 ;;  \::/_/:/  /  \:\  \     /:/\:\  \\::/~~/~~~~  \:\  /:/  /  \::/__//::::\  \
 ;;   \:\/:/  /    \:\  \    \/__\:\  \\:\~~\       \:\/:/  /    \:\  \~~~~\:\  \
 ;;    \::/  /      \:\__\        \:\__\\:\__\       \::/  /      \:\__\    \:\__\
 ;;     \/__/        \/__/         \/__/ \/__/        \/__/        \/__/     \/__/
 ;;            ___          ___          ___          ___          ___
 ;;           /\__\        /\  \        /\  \        /\__\        /\__\
 ;;          /:/ _/_      |::\  \      /::\  \      /:/  /       /:/ _/_
 ;;         /:/ /\__\     |:|:\  \    /:/\:\  \    /:/  /       /:/ /\  \
 ;;        /:/ /:/ _/_  __|:|\:\  \  /:/ /::\  \  /:/  /  ___  /:/ /::\  \
 ;;       /:/_/:/ /\__\/::::|_\:\__\/:/_/:/\:\__\/:/__/  /\__\/:/_/:/\:\__\
 ;;       \:\/:/ /:/  /\:\~~\  \/__/\:\/:/  \/__/\:\  \ /:/  /\:\/:/ /:/  /
 ;;        \::/_/:/  /  \:\  \       \::/__/      \:\  /:/  /  \::/ /:/  /
 ;;         \:\/:/  /    \:\  \       \:\  \       \:\/:/  /    \/_/:/  /
 ;;          \::/  /      \:\__\       \:\__\       \::/  /       /:/  /
 ;;           \/__/        \/__/        \/__/        \/__/        \/__/


;; * Basic personal information
(setq entropy/emacs-user-full-name "Thanos")

(setq entropy/emacs-user-mail-address "thanos@comos.com")

(setq entropy/emacs-indicate-sshd-session nil)

;; * Startup options

;; ;; enable rich dashboard to show recentf and projects but for more loading time
;; (setq entropy/emacs-enable-initial-dashboard 'rich)

;; ;; Automatically center window in some occasions
;; (setq entropy/emacs-align-window-center-automatically-p t)

(setq entropy/emacs-minimal-start nil)

(setq entropy/emacs-custom-enable-lazy-load t)

(setq entropy/emacs-do-pdumper-in-X t)

(setq entropy/emacs-ext-elpkg-customized-get-type
      ;; --- the origin type
         'origin
      ;; --- eemacs spec elpa/melpa mirro (download via 'make intall-eemacs-ext-build')
      ;; 'entropy-emacs-extenisons-project
      )

(setq entropy/emacs-package-archive-repo 'melpa)

;; * Proxy options
;; (setq entropy/emacs-union-http-proxy-plist
;;       '(:enable
;;         t
;;         :host "127.0.0.1"
;;         :port 7890
;;         )
;;       entropy/emacs-union-proxy-noproxy-list
;;       '("127.0.0.1"
;;         "localhost"
;;         ((:type enum :enum_str_list ("192") :sep ".")
;;          (:type enum :enum_str_list ("168") :sep ".")
;;          (:type number_range :range (1 . 3) :sep ".")
;;          (:type number_range :range (1 . 255) :sep "")
;;          )))

;; * Visual basic
(setq entropy/emacs-enable-modeline-toggle t)

;; High performance optimized self-origin type modeline is suggested,
;; but `doom-modeline' has more rich feature but without performance
;; guarantee, so stick on your choice.
(setq entropy/emacs-modeline-style "origin")

;; ;; set initialize frame centering feature
;; (setq entropy/emacs-init-fpos-enable t
;;       entropy/emacs-init-fpos-y 0
;;       entropy/emacs-init-frame-height-scale 0.98
;;       entropy/emacs-init-frame-width-scale 0.75)

;; * IDE options
;; ;; enable eemacs IDE integration
;; (setq entropy/emacs-ide-suppressed nil)
(setq entropy/emacs-ide-use-for-all 'lsp)

;; * _
