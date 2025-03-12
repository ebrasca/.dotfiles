(define-module (config config)
  #:use-module (gnu)
  #:use-module (gnu packages shells)
  #:use-module (gnu system keyboard)
  #:export (system-keyboard-layout
            system-locale
            system-timezone
            system-main-user
            system-main-user-shell
            system-user-list))

;; Shared configuration variables
(define system-keyboard-layout (keyboard-layout "us" "dvp"))
(define system-locale "en_US.utf8")
(define system-timezone "Europe/Warsaw")
(define system-main-user "ebrasca")
(define system-main-user-shell (file-append zsh "/bin/zsh"))

;; Define the main user and append to base user accounts
(define system-user-list
  (append
   (list
    (user-account
     (name system-main-user)
     (group "users")
     (supplementary-groups '("audio" "seat" "video" "wheel"))
     (home-directory (string-append "/home/" system-main-user))
     (shell system-main-user-shell)))
   %base-user-accounts))
