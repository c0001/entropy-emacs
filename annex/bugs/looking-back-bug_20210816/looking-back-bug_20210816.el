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
c2RmYXNkZmRfc2ZkYXNkZmEgXAogICAgICAgICRzZGZhc2Qgc2RoYWxrc2pkaGYgc2FkZm9pdWFz
aGRmYXMgZCBqZm9hc2hkdWZvYXNkaCBmbGFzaGRmb2lhc2RoZmpzYWRob2ZzZGFoZmxzYWRmaGxh
c2QgaHNkZmhzZGhmbGFzIFwKICAgICAgICAiJGFzZGZhc2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2Rm
IgogICAgU0ZIV0lTRFNERl9zZnNhZGZfc2RmYXNkZmRfc2ZkYXNkZmEKfQoKZnVuY3Rpb24gQVNE
RkFTRHFpb3BxaGFzZGhsa2FqQVNIICgpCnsKICAgIGxvY2FsIF9zYWRmYXNkYXM9IiQxIgogICAg
bG9jYWwgc2FkZmFzZGY9IiIKICAgIGFzZmFzZGY9JChldmFsICJlY2hvIFwiJF9zYWRmYXNkZmFz
ZFwiIikKICAgIGlmIFtbICEgLWUgIiRzYWRmYXNkZiIgXV0KICAgIHRoZW4KICAgICAgICBTRkhX
SVNEU0RGX3Nmc2FkZl9zZGZhc2RmZF9zZmRhc2RmYSBcCiAgICAgICAgICAgICJ3dWlvaHMgaHdv
aWFzamhmbGEgd3F1YXNoZmxhaHNmIgogICAgZmkKfQoKZnVuY3Rpb24gQVNERkFTRHFpb3BxaGFz
ZGhsa2FqQVNIICgpCnsKICAgIGxvY2FsIHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2Rm
PSIiCiAgICBsb2NhbCB3aXVoYXNmaGFzamxrZGZodXdpYV9hc2RmYXNkZmFzZGY9IiIKICAgIGxv
Y2FsIHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmX3NkZmFzZGY9IiIKICAgIGxvY2Fs
IHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmX2FzZGZhc2RmYXNkZl9hc2RmYXNkZmFz
ZGY9IiIKICAgIGZvciB3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZiBpbiAke2FzZGZh
c2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2RmW0BdfQogICAgZG8KICAgICAgICB3aXVoYXNmaGFzamxr
ZGZodXdpYV9hc2RmYXNkZmFzZGY9JChBU0RGQVNEcWlvcHFoYXNkaGxrYWpBU0ggIiR3aXVoYXNm
aGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZiIpCiAgICAgICAgU0ZIV0lTRFNERl9zZnNhZGZfc2Rm
YXNkZmRfc2ZkYXNkZmEKICAgICAgICB3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZl9u
YW1lPSIkKGJhc2VuYW1lICIkd2l1aGFzZmhhc2psa2RmaHV3aWFfYXNkZmFzZGZhc2RmIikiCiAg
ICAgICAgU0ZIV0lTRFNERl9zZnNhZGZfc2RmYXNkZmRfc2ZkYXNkZmEgXAogICAgICAgICAgICAi
c2Zhc2Zpd2FzbCB3aXVoYXNmaGFzamxrZGZodXdpYSBpdGVtIDwkd2l1aGFzZmhhc2psa2RmaHV3
aWFfYXNkZmFzZGZhc2RmPnNhZGZhc2RmYXNmYXNkZiIKICAgICAgICB3aXVoYXNmaGFzamxrZGZo
dXdpYWFzZGZhc2RmYXNkZl9uYW1lPSIkd2l1aGFzZmhhc2psa2RmaHV3aWFhc2RmYXNkZmFzZGZf
bmFtZSJfIiRhc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNkZiJfIiR7UkFORE9NfSR7UkFORE9N
fSIKICAgICAgICBhc2RmYXNmYV9zZmRhc19zYWZhc2RmX3NmYXNkZis9KFsiJHdpdWhhc2ZoYXNq
bGtkZmh1d2lhYXNkZmFzZGZhc2RmIl09IiR3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNk
Zl9uYW1lIikKICAgIGRvbmUKfQoKZnVuY3Rpb24gQVNERkFTRHFpb3BxaGFzZGhsa2FqQVNIICgp
CnsKICAgIGxvY2FsIHdpdWhhc2ZoYXNqbGtkZmh1d2lhYXNkZmFzZGZhc2RmPScnCiAgICBsY29h
bCB3aXVoYXNmaGFzamxrZGZodXdpYWFzZGZhc2RmYXNkZl9uYW1lPScnCiAgICBmb3Igd2l1aGFz
Zmhhc2psa2RmaHV3aWFhc2RmYXNkZmFzZGYgaW4gIiR7IWFzZGZhc2ZhX3NmZGFzX3NhZmFzZGZf
c2Zhc2RmW0BdfSIKICAgIGRvCiAgICAgICAgd2l1aGFzZmhhc2psa2RmaHV3aWFhc2RmYXNkZmFz
ZGZfbmFtZT0ke2FzZGZhc2ZhX3NmZGFzX3NhZmFzZGZfc2Zhc2RmWyIkd2l1aGFzZmhhc2psa2Rm
aHV3aWFfIl19CiAgICBkb25lCn0K"
        )
       (buff (get-buffer-create "*bug-test*")))
  (switch-to-buffer buff)
  (with-current-buffer buff
    (erase-buffer)
    (insert thisbug.buffercontent)
    (sh-mode)
    (base64-decode-region (point-min) (point-max))
    (setq company-dabbrev-char-regexp "[-_/a-zA-Z0-9]+")
    (goto-char 2689)
    (message "Bechmark `looking-back' ...")
    (run-with-idle-timer
     2 t
     (lambda ()
       (benchmark-progn
         (looking-back
          (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)"
                  company-dabbrev-char-regexp) ;<---- this regexp freeze looking back of below sh buffer
          (point-at-bol)))))))
