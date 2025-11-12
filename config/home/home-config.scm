(define-module (config home home-config)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages gnupg)
  #:use-module (guix channels))

(home-environment
 (packages
  (specifications->packages
   (list
    ;; Window Management
    "grim"
    "helvum"
    "hyprland"
    "hyprpaper"
    "pavucontrol"
    "pipewire"
    "pulsemixer"
    "slurp"
    "swaynotificationcenter"
    "waybar"
    "wireplumber"
    "wofi"
    "xdg-desktop-portal"
    "xdg-desktop-portal-hyprland"
    ;; Applications
    ;; Browsers
    "firefox"
    ;; Media
    "blender"
    "feh"
    "ffmpegthumbnailer"
    "imagemagick"
    "krita"
    "mediainfo"
    "mpv"
    "obs"
    "obs-vkcapture"
    "obs-wlrobs"
    "poppler"
    ;; Communication
    "gajim"
    "qtox"
    ;; Gaming
    "steam"
    ;; Development
    "cmake"
    "emacs-pgtk"
    "flamegraph"
    "gcc-toolchain"
    "git"
    "git-lfs"
    "libvterm"
    "llama-cpp"
    "make"
    "openssl"
    "pinentry-emacs"
    "sbcl"
    ;; Utilities
    ;; Network
    "nmap"
    "openssh"
    "sshfs"
    "tigervnc-client"
    "wireshark"
    ;; CLI Tools
    "compsize"
    "file"
    "htop"
    "lm-sensors"
    "neofetch"
    "nfs-utils"
    "p7zip"
    "radeontop"
    "setxkbmap"
    "stow"
    "tree"
    "unison"
    "unzip"
    "xlsclients"
    "yt-dlp"
    ;; Security
    "gnupg"
    "passff-host"
    "password-store"
    ;; Terminal
    "alacritty"
    "nushell"
    "screen"
    ;; Virtualization
    "qemu")))
 (services
  (list
   ;; Core Services:
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   ;; Guix Channels:
   (simple-service 'guixrus-service
                   home-channels-service-type
                   (list
                    (channel
                     (name 'nonguix)
                     (url "https://gitlab.com/nonguix/nonguix")
                     ;; Enable signature verification:
                     (introduction
                      (make-channel-introduction
                       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                       (openpgp-fingerprint
                        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))
   ;; GPG Agent:
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)))
   ;; Shell Configuration:
   (service home-zsh-service-type
            (home-zsh-configuration
             (zprofile
              (list (local-file "../../.zprofile" "zprofile")))
             (zshrc
              (list (local-file "../../.zshrc" "zshrc"))))))))
