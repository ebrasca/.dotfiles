(in-package :stumpwm)

(ql:quickload '(:swank
                :trivial-shell
                :cl-ppcre))

(init-load-path *module-dir*)

(run-shell-command "gentoo-pipewire-launcher")

(defun utl-load (filename)
  (let ((file (concat "~/.stumpwm.d/" filename ".lisp")))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(loop :for file :in '("utils"
                      "config"
                      "keys"
                      "commands"
                      "private")
      :do (utl-load file))
