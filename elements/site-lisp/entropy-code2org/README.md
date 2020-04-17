- [Copyright (C) 20200417  Entropy](#orgbbfd280)
- [Commentary:](#org3ad5444)
- [Configuration:](#org882259d)
- [Code:](#org951c37b)
- [provide](#orgd507585)



<a id="orgbbfd280"></a>

# Copyright (C) 20200417  Entropy

    Author:        Entropy <bmsac0001@gmail.com>
    Maintainer:    Entropy <bmsac001@gmail.com>
    URL:           url
    Package-Version: 0.1.0
    Version:       0.1.0
    Created:       2020-04-17 01:59:47
    Keywords:      org, outline
    Compatibility: GNU Emacs emacs-version;
    Package-Requires: ((emacs "24") (cl-lib "0.5"))

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


<a id="org3ad5444"></a>

# Commentary:

Convert your source buffer into an org context.

Usage:

-   **`entropy/code2org-export-cur-to-README`:** convert current buffer to README as markdown format.
-   **`entropy/code2org-export-cur-to-org-file`:** convert current buffer to a org file.
-   **`entropy/code2org-export-cur-to-html-file`:** convert current buffer to a html file.
-   **`entropy/code2org-convert-current-buffer`:** convert current buffer to a temporal buffer with org context.


<a id="org882259d"></a>

# Configuration:

Just `(require 'entropy-code2org)`


<a id="org951c37b"></a>
