(require 'package)
(package-initialize)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(json yaml-mode treemacs)
