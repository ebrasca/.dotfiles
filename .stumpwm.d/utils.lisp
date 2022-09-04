(in-package :stumpwm)

(defmacro ret (var val &body body)
  "It is a blend of let and return"
  `(let ((,var ,val))
     ,@body
     ,var))

(defun modeline-date ()
  (run-shell-command "date '+%F %H:%M'| tr -d [:cntrl:]" t))

(defun set-volume (n)
  (when (<= n 100)
    (setf *volume* n)
    (run-commands (format nil "exec pactl -- set-sink-volume 0 ~s%" n))))
