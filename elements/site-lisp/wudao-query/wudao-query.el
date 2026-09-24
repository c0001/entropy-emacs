;; -*- lexical-binding: t; -*-
;; * code
(require 'wudao-lib)
(require 'wudao-json-cache)

;;;###autoload
(defun wudao/query-word-by-hash/use-json-parse (query &optional full)
  (wudao/json-cache-query-by-cache query (not full)))
;;;###autoload
(defun wudao/query-word-by-command/use-json-parse (query &optional full)
  (wudao/json-cache-query-by-command query (not full)))

;; * provide
(provide 'wudao-query)
