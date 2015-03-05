(setq default-frame-alist '(
  (background-color . "cornsilk")
  (width . 100)
  (height . 52)
))
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")  package-archives)
(push '("melpa"     . "http://melpa.milkbox.net/packages/")   package-archives)

(add-to-list 'load-path "~/.emacs.d/elpa/evil-1.0.8")
(require 'evil)
(evil-mode 1)
