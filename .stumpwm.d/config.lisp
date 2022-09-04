(in-package :stumpwm)

(defparameter *web-browser* "firefox")

;; bugfix for scrolling doesn't work with an external mouse in GTK+3 apps.
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

(setf *screen-mode-line-format* (list '(:eval (modeline-date)) " [^B%n^b] %W")
      *mode-line-timeout* 5
      *group-format* " %n%s%t "
      *startup-message* (machine-instance)
      *mouse-focus-policy* :ignore)

(defvar *volume* 50)

(set-volume *volume*)

;; Turn on the modeline
(unless (head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head)))

(run-commands "grename F1"
              "gnewbg F2"
              "gnewbg F3"
              "gnewbg F4"
              "gnewbg F5"
              "gnewbg F6"
              "gnewbg F7"
              "gnewbg F8"
              "gnewbg F9")
(run-commands "gselect 1")
