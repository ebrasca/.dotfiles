(define-module (config systems ebrasca)
  #:use-module (config config)
  #:use-module (config systems base-system)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services nfs)
  #:use-module (gnu services linux)
  #:use-module (gnu services vpn)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

;; Operating system configuration for ebrasca
(operating-system
 (inherit base-system)
 (packages
  (append (specifications->packages
           (list "nfs-utils"
                 "wireguard-tools"
                 "xf86-video-amdgpu"))
          %base-packages))
 (services
  (append
   (list
    (service linux-services:earlyoom-service-type)
    ;; File System Services
    (service fstrim-service-type)
    (service nfs-service-type
             (nfs-configuration))
    ;; Network Services:
    (service static-networking-service-type
             (list (static-networking
                    (addresses
                     (list (network-address
                            (device "enp8s0")
                            (value "192.168.10.2/24"))))
                    (routes
                     (list (network-route
                            (destination "default")
                            (gateway "192.168.10.1")))))))
    (service tor-service-type)
    (service wireguard-service-type
             (wireguard-configuration
              (addresses '("10.0.0.3/24"))
              (peers
               (list
                (wireguard-peer
                 (name "TalosII")
                 (endpoint "171.25.198.58:51820")
                 (public-key "6tZyIItXjmQXvoyBrAZRm/Xmcbc4vuVti7odD8AUgQg=")
                 (allowed-ips '("10.0.0.0/24"))))))))
   system-base-services))
 (kernel linux)
 (kernel-arguments
  '(;; ─── Boot and General ─────────────────────────────────────────────
    "quiet"                             ; Minimize boot output
    "splash"                            ; Graphical splash screen
    "noatime"                           ; Disable file access time updates
    ;; ─── CPU and Memory Security ─────────────────────────────────────
    "kptr_restrict=2"                   ; Hide kernel pointers
    "lockdown=confidentiality"          ; Kernel lockdown
    "module.sig_enforce=1"              ; Enforce signed modules
    "page_alloc.shuffle=1"              ; Enable randomize page allocator
    "preempt=full"                      ; Full preemption
    "pti=on"                            ; Enable kernel Page Table Isolation
    "randomize_kstack_offset=on"        ; Randomize kernel stack
    "transparent_hugepage=always"       ; Enable hugepages
    "vsyscall=none"                     ; Disable vsyscall
    ;; ─── AMD GPU Tuning ─────────────────────────────────────────────
    ;;"amdgpu.ppfeaturemask=0xffffffff" ; Unlock all features
    ;; "amdgpu.gpu_recovery=1"          ; GPU recovery
    ;;"amdgpu.dcfeaturemask=0xffffffff" ; All Display Core features
    ;; ─── IOMMU and Virtualization ───────────────────────────────────
    "amd_iommu=on"                   ; Enable AMD IOMMU
    "iommu=pt"                       ; Passthrough mode
    ))
 (initrd microcode-initrd)
 (firmware (list linux-firmware amdgpu-firmware))
 (kernel-loadable-modules (list v4l2loopback-linux-module))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout system-keyboard-layout)))
 (file-systems
  (append
   (list (file-system
          (mount-point "/")
          (device (uuid "442f0f54-7c9f-43b3-868e-bdbdbbefd1dc"
                        'btrfs))
          (type "btrfs")
          (options "compress-force=zstd:3,ssd,subvol=/"))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "EA15-6EDE"
                        'fat32))
          (type "vfat"))
         ;; TODO
         ;; (file-system
         ;;  (mount-point "/mnt/nfs")
         ;;  (device "IP:MOUNT_POINT")
         ;;  (type "nfs")
         ;;  (mount? #f)
         ;;  (create-mount-point? #t)
         ;;  (options "soft,timeo=100,rsize=32768,wsize=32768")
         ;;  (flags '(no-atime)))
         )
   %base-file-systems)))
