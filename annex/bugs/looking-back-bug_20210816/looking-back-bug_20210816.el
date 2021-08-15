(require 'benchmark)
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)

;; (package-initialize)
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                          ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;;                          ))
;; (package-refresh-contents)
;; (setq inhibit-startup-screen t)
;; (dolist (pkg '(company))
;;   (package-install pkg))



(let* (;; (thisbug.srcdir
       ;;  (expand-file-name
       ;;   (file-name-directory load-file-name)))
       ;; (thisbug.testfile
       ;;  (expand-file-name "test.sh" thisbug.srcdir))
       (thisbug.buffercontent
        "ZGVjbGFyZSAtYSBhc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNkZj0oCiAgICAiXCR7SE9NRX0v
LmNvbmZpZy94eHh4IgogICAgIlwke0hPTUV9Ly5jb25maWcvYXNkZmFzZGZhc2QiCiAgICAiXCR7
SE9NRX0vLmNvbmZpZy9zZmFzZGZhc2RmYXNkIgopCmRlY2xhcmUgLUEgYXNkZmFzZmFfc2ZkYXNf
c2FmYXNkZl9zZmFzZGY9KCkKCmRlY2xhcmUgYXNkZmFzZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGY9
JChTRkhXSVNEU0RGKQoKZGVjbGFyZSBhc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNkZj0iL3Nk
ZmFzZGZhc2ZJU0RGSS9TREZBU0QtU0FERkFTREYvU0FERkFTREZfU0RGQVNEX1NGREFTRi9hc2Rm
YXNmYXNmYXMvIgpkZWNsYXJlIGFzZGZhc2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2RmPSIkYXNkZmFz
ZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGYiL1NERkFTREZfc2RmX3NhZGZhc2RmXyIke2FzZGZhc2Zh
X3NmZGFzX3NhZmFzZGZfc2Zhc2RmfSJfc2RmYXNkZmFzCmRlY2xhcmUgYXNkZmFzZmFfc2ZkYXNf
c2FmYXNkZl9zZmFzZGY9IiRhc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNkZiIvc2RmYXNTRHNk
RFNGc2Rmc2RmLnR4dAoKZnVuY3Rpb24gQVNERkFTRHFpb3BxaGFzZGhsa2FqQVNIICgpCnsKICAg
IHdoaWxlIFtbIC1kICIkYXNkZmFzZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGYiIF1dCiAgICBkbwog
ICAgICAgIGFzZGZhc2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2RmPSIkYXNkZmFzZmFfc2ZkYXNfc2Fm
YXNkZl9zZmFzZGYiL3NkZmFzZGZhc2ZzZF8iJHthc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNk
Zn0iXyIke1JBTkRPTX0ke1JBTkRPTX0iCiAgICBkb25lCgogICAgU0ZIV0lTRFNERl9zZnNhZGZf
c2RmYXNkZmRfc2ZkYXNkZmEgXAogICAgICAgICRUT1BfREFTSDBfRVhQT1JUX0RPX0FTX1RFU1Qg
XAogICAgICAgICIkYXNkZmFzZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGYiCiAgICBTRkhXSVNEU0RG
X3Nmc2FkZl9zZGZhc2RmZF9zZmRhc2RmYQp9CgpmdW5jdGlvbiBBU0RGQVNEcWlvcHFoYXNkaGxr
YWpBU0ggKCkKewogICAgbG9jYWwgX3NhZGZhc2Rhcz0iJDEiCiAgICBsb2NhbCBzYWRmYXNkZj0i
IgogICAgYXNmYXNkZj0kKGV2YWwgImVjaG8gXCIkX3NhZGZhc2RmYXNkXCIiKQogICAgaWYgW1sg
ISAtZSAiJHNhZGZhc2RmIiBdXQogICAgdGhlbgogICAgICAgIFNGSFdJU0RTREZfc2ZzYWRmX3Nk
ZmFzZGZkX3NmZGFzZGZhIFwKICAgICAgICAgICAgInd1aW9ocyBod29pYXNqaGZsYSB3cXVhc2hm
bGFoc2YiCiAgICBmaQp9CgpmdW5jdGlvbiBBU0RGQVNEcWlvcHFoYXNkaGxrYWpBU0ggKCkKewog
ICAgbG9jYWwgd2l1aGFzZmhhc2psa2RmaHV3aWFhc2RmYXNkZmFzZGY9IiIKICAgIGxvY2FsIHdp
dWhhc2ZoYXNqbGtkZmh1d2lhX2FzZGZhc2RmYXNkZj0iIgogICAgbG9jYWwgd2l1aGFzZmhhc2ps
a2RmaHV3aWFhc2RmYXNkZmFzZGZfc2RmYXNkZj0iIgogICAgbG9jYWwgd2l1aGFzZmhhc2psa2Rm
aHV3aWFhc2RmYXNkZmFzZGZfYXNkZmFzZGZhc2RmX2FzZGZhc2RmYXNkZj0iIgogICAgZm9yIHdp
dWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmIGluICR7YXNkZmFzZmFfc2ZkYXNfc2FmYXNk
Zl9zZmFzZGZbQF19CiAgICBkbwogICAgICAgIHdpdWhhc2ZoYXNqbGtkZmh1d2lhX2FzZGZhc2Rm
YXNkZj0kKEFTREZBU0RxaW9wcWhhc2RobGthakFTSCAiJHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNk
ZmFzZGZhc2RmIikKICAgICAgICBTRkhXSVNEU0RGX3Nmc2FkZl9zZGZhc2RmZF9zZmRhc2RmYQog
ICAgICAgIHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmX25hbWU9IiQoYmFzZW5hbWUg
IiR3aXVoYXNmaGFzamxrZGZodXdpYV9hc2RmYXNkZmFzZGYiKSIKICAgICAgICBTRkhXSVNEU0RG
X3Nmc2FkZl9zZGZhc2RmZF9zZmRhc2RmYSBcCiAgICAgICAgICAgICJzZmFzZml3YXNsIHdpdWhh
c2ZoYXNqbGtkZmh1d2lhIGl0ZW0gPCR3aXVoYXNmaGFzamxrZGZodXdpYV9hc2RmYXNkZmFzZGY+
c2FkZmFzZGZhc2Zhc2RmIgogICAgICAgIHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2Rm
X25hbWU9IiR3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZl9uYW1lIl8iJGFzZGZhc2Zh
X3NmZGFzX3NhZmFzZGZfc2Zhc2RmIl8iJHtSQU5ET019JHtSQU5ET019IgogICAgICAgIGFzZGZh
c2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2RmKz0oWyIkd2l1aGFzZmhhc2psa2RmaHV3aWFhc2RmYXNk
ZmFzZGYiXT0iJHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmX25hbWUiKQogICAgZG9u
ZQp9CgpmdW5jdGlvbiBBU0RGQVNEcWlvcHFoYXNkaGxrYWpBU0ggKCkKewogICAgbG9jYWwgd2l1
aGFzZmhhc2psa2RmaHV3aWFhc2RmYXNkZmFzZGY9JycKICAgIGxjb2FsIHdpdWhhc2ZoYXNqbGtk
Zmh1d2lhYXNkZmFzZGZhc2RmX25hbWU9JycKICAgIGZvciB3aXVoYXNmaGFzamxrZGZodXdpYWFz
ZGZhc2RmYXNkZiBpbiAiJHshYXNkZmFzZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGZbQF19IgogICAg
ZG8KICAgICAgICB3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZl9uYW1lPSR7YXNkZmFz
ZmFfc2ZkYXNfc2FmYXNkZl9zZmFzZGZbIiR3aXVoYXNmaGFzamxrZGZodXdpYV8iXX0KICAgIGRv
bmUKfQo="
        )
       (buff (get-buffer-create "*bug-test*")))
  (switch-to-buffer buff)
  (with-current-buffer buff
    (erase-buffer)
    (insert thisbug.buffercontent)
    (sh-mode)
    (base64-decode-region (point-min) (point-max))
    (setq company-dabbrev-char-regexp "[-_/a-zA-Z0-9]+")
    (goto-char 2613)
    (message "Bechmark `looking-back' ...")
    (run-with-idle-timer
     2 t
     (lambda ()
       (benchmark-progn
         (looking-back
          (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)"
                  company-dabbrev-char-regexp) ;<---- this regexp freeze looking back of below sh buffer
          (point-at-bol)))))))
