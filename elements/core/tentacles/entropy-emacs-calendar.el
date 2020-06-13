;;; entropy-emacs-calendar.el --- entropy-emacs calendar config
;;
;; * Copyright (C) 20190603  Entropy
;; #+BEGIN_EXAMPLE
;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           https://github.com/c0001/entropy-emacs/blob/master/elements/entropy-emacs-calendar.el
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
;; Calendar config for entropy-emacs appending with chinese holidays
;; information.
;;

;; * Configuration:
;;
;; Loading by `entropy-emacs' automatically.
;;
;; * Code:

;; ** require
(require 'entropy-emacs-defconst)
(require 'entropy-emacs-defcustom)
(require 'entropy-emacs-defun)

;; ** main

(use-package cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init
  (entropy/emacs-lazy-with-load-trail
   china-calendar-init
   (cal-china-x-setup))
  :config
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7  "七夕节")
                                       (holiday-fixed 3 8  "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4  "青年节")
                                       (holiday-fixed 6 1  "儿童节")
                                       (holiday-fixed 9 10 "教师节")
                                       (holiday-fixed 10 1 "国庆节")
                                       (holiday-fixed 11 11 "双十一购物节")
                                       )
        holiday-other-holidays '((holiday-fixed 2 14       "情人节")
                                 (holiday-fixed 4 1        "愚人节")
                                 (holiday-fixed 12 25      "圣诞节")
                                 (holiday-float 5 0 2      "母亲节")
                                 (holiday-float 6 0 3      "父亲节")
                                 (holiday-float 11 4 4     "感恩节")
                                 )
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  holiday-other-holidays)))

(provide 'entropy-emacs-calendar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
