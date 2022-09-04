(in-package :stumpwm)

;;; clear keys
(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-m"))

;;; *top-map*
(set-prefix-key (kbd "C-."))

;;; Controlling sound
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl -- set-sink-mute 0 toggle")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "increase-volume")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "decrease-volume")

;;; *root-map*
(define-key *root-map* (kbd "x") '*app-map*)
(define-key *root-map* (kbd "q") "exit")

(defparameter *app-map*
  (ret m (stumpwm:make-sparse-keymap)
    (define-key m (kbd "e") "emacs/normal")
    (define-key m (kbd "i") "emacs/erc")
    (define-key m (kbd "m") "emacs/gnus")
    (define-key m (kbd "c") "xterminal")
    (define-key m (kbd "f") "firefox")
    (define-key m (kbd "s") "swank")))
