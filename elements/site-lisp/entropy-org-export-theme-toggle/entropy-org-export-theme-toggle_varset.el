;;; entropy-org-export-theme-toggle_varset.el --- varaible default value setting

;; Copyright (C) 2018-11-12 Entropy

;; Author:        Entropy <bmsac0001@gmail.com>
;; Maintainer:    Entropy <bmsac001@gmail.com>
;; URL:           none
;; Package-Version: none
;; Version:       none
;; Compatibility: GNU Emacs emacs-version;
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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


;;; Code:

(setq entropy/org-exptt-html-theme-plist
      '(("Worg"
         :theme_css  ("<link rel=\"stylesheet\" title=\"Standard\" href=\"https://orgmode.org/worg/style/worg.css\" type=\"text/css\" />"
                      "<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"https://orgmode.org/worg/style/worg-zenburn.css\" type=\"text/css\" />"
                      "<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"https://orgmode.org/worg/style/worg-classic.css\" type=\"text/css\" />"
                      )
         :theme_js nil
         :theme_mischellaneous ("<link rel=\"SHORTCUT ICON\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/x-icon\" />"
                                "<link rel=\"icon\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/ico\" />")
         :theme_category ("index" "sub-file")
         :theme_dontcache t)
        ("read-the-org"
         :theme_css ("<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>"
                     "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>"
                     "
<style>
    pre.src
    {
      overflow-x: scroll;
      overflow-y: scroll;
      max-height: 400px;
    }

    pre.example
    {
      overflow-x: scroll;
      overflow-y: scroll;
      max-height: 400px;
    }


    #content img{max-width: 500px}

    .org-svg
    {
      max-width: 500px
    }
</style>"
                     )
         :theme_js  ("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>"
                     "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>"
                     "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.min.js\"></script>"
                     "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>")
         :theme_category ("single-doc" "wiki"))
        ("Bigblow"
         :theme_css ("<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.pirilampo.org/styles/bigblow/css/htmlize.css\"/>"
                     "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.pirilampo.org/styles/bigblow/css/bigblow.css\"/>"
                     "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.pirilampo.org/styles/bigblow/css/hideshow.css\"/>"
                     "
<style>
    pre.src
    {
      overflow-x: scroll;
      overflow-y: scroll;
      max-height: 400px;
    }

    pre.example
    {
      overflow-x: scroll;
      overflow-y: scroll;
      max-height: 400px;
    }


    img{max-width: 500px}

    .org-svg
    {
      max-width: 500px
    }
</style>"
                     )
         :theme_js ("<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-1.11.0.min.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery-ui-1.10.2.min.js\"></script>"

                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.localscroll-min.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.scrollTo-1.4.3.1-min.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/jquery.zclip.min.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/bigblow.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/bigblow/js/hideshow.js\"></script>"
                    "<script type=\"text/javascript\" src=\"https://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.min.js\"></script>")
         :theme_category ("index" "single-doc" "wiki"))
        ("redsnapper"
         :theme_css ("<link rel=\"stylesheet\" style=\"text/css\"
href=\"https://raw.githubusercontent.com/c0001/entropy-org-export-html-theme/master/elements/themes/redsnapper-theme/org-redsnapper-html.css\" />")
         :theme_js nil
         :theme_category ("single-doc" "wiki"))
        ("redsnapper-index"
         :theme_js nil
         :theme_css ("<link rel=\"stylesheet\" style=\"text/css\"
href=\"https://raw.githubusercontent.com/c0001/entropy-org-export-html-theme/master/elements/themes/redsnapper-theme/org-redsnapper-html-index.css\" />")
         :theme_category ("index"))))


(provide 'entropy-org-export-theme-toggle_varset)
