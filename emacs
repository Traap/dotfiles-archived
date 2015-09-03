  (setq default-frame-alist '((width . 100) (height . 52)))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 
     'package-archives 
    '("melpa" . "http://melpa.org/packages/")
    t)
  (package-initialize))
