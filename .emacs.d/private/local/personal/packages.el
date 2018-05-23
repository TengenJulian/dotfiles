;;; packages.el --- personal Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst personal-packages
  '(
    ;; package personals go here
    evil
    evil-surround
    ace-window
    dired
    (personal :location local)
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar personal-excluded-packages '(flycheck-pos-tip)
  "List of packages to exclude.")

(defun personal/init-personal ()
  "Initialize my extension"
  (message "trying to load personal")
  (use-package personal
    :config
    (progn
                                        ; (evil-leader/set-key "onl" 'personal-ido-select)
      )
    )
  )
(defun personal/post-init-evil ()
  )
(defun personal/post-init-evil-surround ()
  )
(defun personal/post-init-ace-window ()
  )
(defun personal/post-init-dired ()
  )


;; For each package, define a function personal/init-<package-personal>
;;
;; (defun personal/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
