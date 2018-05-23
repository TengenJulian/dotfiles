;;; ability to set directory of modes
;;; This folder has the following structure
;;; <mmn-folder>/
;;;    - modes
;;;       - <major mode name>.org
;;;    - files
;;;       - <file name>.org
;;;    - misc
;;;       - <misc name>.org

;;; function to open current major mode notebook
;;; function to choose one of the minor mode notebook
;;; function to open ido-complete with notebooks

;;; keybinding for the above functions
(require 'f)
(require 'dash)
(require 'dbus)
(require 'subr-x)
(require 'projectile)

(defgroup mmnote nil " Major Minor Note group.")

(defcustom mmnote-folder "~/Dropbox/notes"
  "The folder location of the note files."
  :type '(file :must-match t)
  :group 'mmnote
  )

(defun mmnote-get-folder (folder) (f-join mmnote-folder folder))

(defun mmnote-open-or-create-file (filename)
  (interactive)
  (let ((filepath (f-join mmnote-folder filename)))
    (cond
     ((eq filepath nil) (message "could not open file"))
     ((not (f-exists? filepath)) (f-touch filepath))
     (t (find-file filepath)))
    ))

(defun mmnote-make-default-dirs ()
  (interactive)
  (let ((dirs (-map 'mmnote-get-folder '("modes" "files" "misc" "projects"))))
    (-each dirs
      (lambda (dir)
        (unless (f-exists? dir) (f-mkdir dir))))
    )
  )

(defun mmnote-open-notebook (type &optional name)
  (cond ((string= type "modes") (mmnote-open-mode-notebook name))
        ((string= type "files") (mmnote-open-file-notebook name))
        ((string= type "misc")  (mmnote-open-misc-notebook name))
        ))

(defun personal-current-dir ()
  (or
   (file-name-directory (or "" (buffer-file-name)))
   (file-truename default-directory)))

(defun mmnote-open-mode-notebook (&optional o-name)
  (interactive)
  (mmnote-make-default-dirs)
  (let ((name (if o-name o-name (format "%s.org" major-mode))))
    (mmnote-make-default-dirs)
    (mmnote-open-or-create-file (f-join (mmnote-get-folder "modes") name))))

(defun mmnote-open-misc-notebook (name)
  (interactive "sNotebook Name: ")
  (mmnote-make-default-dirs)
  (mmnote-open-or-create-file (f-join (mmnote-get-folder "misc") name)))

(defun mmnote-open-project-notebook (&optional name)
  (interactive)
  (mmnote-make-default-dirs)
  (let (folder)
    (condition-case ex
        (setq folder (projectile-project-root))
      ('error (message "Not in a project")))

    (let* (
          (hash (md5 folder))
          (filename (format "%s-%s.org" (f-filename folder) hash))
          )
      (mmnote-open-or-create-file (f-join (mmnote-get-folder "projects") filename))
      )
    )
  )

(defun mmnote-open-file-notebook (&optional o-fullpath)
  (interactive)
  (mmnote-make-default-dirs)
  (let ((fullpath (if o-fullpath o-fullpath (buffer-file-name))))
    (if (eq fullpath nil)
        (message "Could not open file.")
      (let ((filename (f-filename fullpath))
            (hash (md5 fullpath)))
        (mmnote-open-or-create-file (f-join (mmnote-get-folder "misc")
                                            (format "%s-%s.org" filename hash))))
      )))

(defun mmnote-ido-select ()
  (interactive)
  (let ((file-map (make-hash-table :test 'equal)))
    (-each (f-files (mmnote-get-folder "modes"))
      (lambda (path) (puthash path "modes" file-map))
      )
    (-each (f-files (mmnote-get-folder "files"))
      (lambda (path) (puthash path "modes" file-map))
      )
    (-each (f-files (mmnote-get-folder "misc"))
      (lambda (path) (puthash path "modes" file-map))
      )
    (let ((selection (ido-completing-read "Select a notebook: " (hash-table-keys file-map))))
      (mmnote-open-notebook (gethash selection file-map) selection)
      )))

(defun mmnote-new-misc-notebook (filename)
  (interactive "sNew notebook name: ")
  (mmnote-open-or-create-file (f-join (mmnote-get-folder "misc") filename))
  )

(define-minor-mode mmnote-mode
  "Mayor Minor note making mode."
  :lighter " mmn"
  :global t
  (mmnote-make-default-dirs))

(provide 'mmnote)
