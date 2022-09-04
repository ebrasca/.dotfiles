(in-package :stumpwm)

(defcommand increase-volume () ()
  "Increase volume by 5%"
  (set-volume (+ *volume* 5)))

(defcommand decrease-volume () ()
  "Decrease volume by 5%"
  (set-volume (- *volume* 5)))

(defcommand emacs/normal () ()
  "Run or raise emacs/normal."
  (run-or-raise "emacs --name emacs/normal" '(:title "emacs/normal")))

(defcommand emacs/erc () ()
  "Run or raise emacs/erc."
  (run-or-raise "emacs --eval '(erc)' --name emacs/erc" '(:title "emacs/erc")))

(defcommand emacs/gnus () ()
  "Run or raise emacs/gnus."
  (run-or-raise "emacs --eval '(gnus)' --name emacs/gnus" '(:title "emacs/gnus")))

(defcommand firefox () ()
  "Start or switch to web-browser."
  (run-or-raise *web-browser* '(:instance "Navigator" :role "browser")))

(defcommand xterminal () ()
  "Start or raise xterminal."
  (run-shell-command "alacritty"))

(defcommand exit (y-n) ((:y-or-n "Exit stumpwm?"))
  "Exit stumpwm"
  (when y-n
    (cl-user::exit)))

(let ((server-running nil))
  (defcommand swank () ()
    "Toggle the swank server on/off"
    (cond (server-running
           (swank:stop-server 4006)
           (echo-string (current-screen) "Stopping swank.")
           (setf server-running nil))
          (t
           (swank:create-server :port 4006
                                :style swank:*communication-style*
                                :dont-close t)
           (echo-string (current-screen) "Starting swank.")
           (setf server-running t)))))
