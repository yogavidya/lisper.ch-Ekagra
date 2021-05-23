(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(add-to-list 'load-path "~/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
