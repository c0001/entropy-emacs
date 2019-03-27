(require 'url-util)

(defconst entropy/cl-url--allowed-chars
  (url--allowed-chars (let ((hl url-unreserved-chars)
                            (x 1))
                        (dolist (el '(37 38 47 58 61 63))
                          (add-to-list 'hl el))
                        hl))
  "Allowed chars ': /' for url-protocal heads in function
`url-hexify-string'")

(defconst entropy/clconst-cjk-punc-regexp
  "[\u3002\uff1b\uff0c\uff1a\u201c\u201d\uff08\uff09\u3001\uff1f\u300a\u300b]"
  "CJK punctuation regexp presentation")

(defconst entropy/clconst-cjk-char-regexp
  "[\u4e00-\u9fa5]"
  "CJK char regexp presentation")

(provide 'entropy-common-library-const)
