;;; -*- lexical-binding: t -*-

;;; keybinding for the above functions
(require 'dbus)

;;; Evil functions
(defun personal-current-dir ()
  (or
   (file-name-directory (or "" (buffer-file-name)))
   (file-truename default-directory)))

(defun personal-f-buffer(f)
  (interactive)
  (funcall f)
  (let ((done nil))
    (while (not done)
      (funcall f)
      (setq done (not (string= major-mode "dired-mode")))
      )))

(defun dbug-personal-current-dir ()
  ;; you need to map between dbus and emacs datatypes, that's what :string is for
  ;; if you're returning just one value that should work automatically, otherwise
  ;; you're expected to put your return values in a list like I am doing here
  (list :string (personal-current-dir))
  )

(dbus-register-method
 :session
 "org.test.emacs"
 "/personal"
 "org.test.emacs"
 "current_dir"
 'dbug-personal-current-dir)

(defun dbus-test-slash-introspect ()
  "<node name='/'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>
  <node name='personal'>
  </node>
  </node>")

(dbus-register-method
 :session
 "org.test.emacs"
 "/"
 dbus-interface-introspectable
 "Introspect"
 'dbus-test-slash-introspect)

(defun dbus-personal-current-dir-introspect ()
  "<node name='/personal'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>
  <interface name='org.test.emacs'>
  <method name='current_dir'>
  <arg name='' direction='out' type='s' />
  </method>
  </interface>
  </node>")

(dbus-register-method
 :session
 "org.test.emacs"
 "/personal"
 dbus-interface-introspectable
 "Introspect"
 'dbus-personal-current-dir-introspect)

(provide 'personal)
