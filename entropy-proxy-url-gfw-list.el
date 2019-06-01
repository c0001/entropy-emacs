;;; entropy-proxy-url-gfw-list --- using gfw list config proxy regular

;; Copyright (C) 2018-09-23  Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           None
;; Package-Version: None
;; Version:       None
;; Created:       2018-09-23 22:19:37
;; Keywords:      None
;; Compatibility: GNU Emacs 25;
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Using gfw list configing entropy-proxy-url, gfw list maintained in
;; https://github.com/gfwlist/gfwlist.
;;
;;; Configuration:
;;
;; None need to using it manually.

;;; Code:
(require 'entropy-adblock+-rule-analysis)
(setq entropy/proxy-url-regexp-list
      (entropy/adbp-rule-get-regexp-matchs-list))


(provide 'entropy-proxy-url-gfw-list)
