;;; extensions.el --- mmnote Layer extensions File for Spacemacs
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

(setq mmnote-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq mmnote-post-extensions
      '(
        mmnote
        ;; post extension names go here
        ))

(defun mmnote/init-mmnote ()
  "Initialize my extension"
  (message "trying to load mmnote")
  (use-package mmnote
    :config
    (progn
      (evil-leader/set-key "onl" 'mmnote-ido-select)
      (evil-leader/set-key "onp" 'mmnote-open-project-notebook)
      (evil-leader/set-key "onf" 'mmnote-open-file-notebook)
      (evil-leader/set-key "onm" 'mmnote-open-mode-notebook)
      )
    )
  )

;; For each extension, define a function mmnote/init-<extension-name>
;;
;; (defun mmnote/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
