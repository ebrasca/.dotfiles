(define-module (config systems base-system)
  #:use-module (config config)
  #:use-module (gnu)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:export (system-base-services
            base-system))

;; Base services for the system
(define system-base-services
  (append
   (list
    (service greetd-service-type
             (greetd-configuration
              (greeter-supplementary-groups (list "video" "input" "seat"))
              (terminals
               (list
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "1")
                 (terminal-switch #t)
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "2")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "3")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "4")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "5")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))
                (greetd-terminal-configuration
                 (extra-shepherd-requirement '(seatd))
                 (terminal-vt "6")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command system-main-user-shell))))))))))
    ;; Time Services:
    (service ntp-service-type)
    ;; Desktop Services:
    (service dbus-root-service-type)
    (service seatd-service-type)
    fontconfig-file-system-service ;; <<<
    ;; Hardware Services:
    (service upower-service-type)
    ;; User Services:
    (service accountsservice-service-type))
   (modify-services %base-services
                    ;; greetd-service-type provides "greetd" PAM service
                    (delete login-service-type)
                    ;; and can be used in place of mingetty-service-type
                    (delete mingetty-service-type))))

;; Base operating system configuration
(define base-system
  (operating-system
   (keyboard-layout system-keyboard-layout)
   (locale system-locale)
   (timezone system-timezone)
   (host-name system-main-user)
   (users system-user-list)
   (packages %base-packages)
   (services system-base-services)
   ;; Don't include any default firmware
   (firmware '())
   (initrd (lambda (file-systems . rest)
             ;; Create a standard initrd but set up networking
             ;; with the parameters QEMU expects by default.
             (apply base-initrd file-systems
                    #:qemu-networking? #t
                    rest)))
   ;; The bootloader and file-systems fields here will be replaced by
   ;; the exact same values in the gemini and taurus configurations,
   ;; but in practice these fields will depend on each machine's
   ;; partition configuration.
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))
                (terminal-outputs '(console))))
   (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                       %base-file-systems))))
