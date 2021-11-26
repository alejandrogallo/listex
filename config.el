(require 'package)

(setq package-enable-at-startup t)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"   )
        ("org"   . "http://orgmode.org/elpa/"     )))
(message "Initializing packages")
(package-initialize)

(message "Setting up use-package")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(message "Requiring use-package")
(eval-when-compile
  (require 'use-package))

(message "requiring org")
(require 'org)

(message "up org-contrib")
(use-package org-plus-contrib
  :defer t
  :ensure t
  :config
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(message "htmlize")
(use-package htmlize
  :defer t
  :ensure t)
