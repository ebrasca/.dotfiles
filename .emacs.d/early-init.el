;;;-----------------------------------------------------------------------------
;;; Basic Emacs Settings
;;;-----------------------------------------------------------------------------

(setq-default ad-redefinition-action 'accept         ; Silence warnings for redefinition
              auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
              cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
              cursor-type '(hbar . 2)                ; Underline-shaped cursor
              custom-file "/dev/null"                ; Disable customize-*
              fill-column 80                         ; Set width for automatic line breaks
              frame-resize-pixelwise t               ; Disable frame
              gc-cons-threshold (* 8 1024 1024)      ; Increase garbage collection threshold
              help-window-select t                   ; Focus new help windows when opened
              indent-tabs-mode nil                   ; Stop using tabs to indent
              inhibit-default-init t
              inhibit-startup-screen t               ; Disable start-up screen
              initial-scratch-message ""             ; Empty the initial *scratch* buffer
              large-file-warning-threshold nil       ; Silence warnings for large files
              mouse-yank-at-point t                  ; Yank at point rather than pointer
              package-enable-at-startup nil          ; Read the init file before loading packages
              package-quickstart nil                 ; Prevent package.el loading packages prior to their init-file
              read-process-output-max (* 1024 1024)  ; Increase read size for data chunks
              show-help-function nil                 ; Disable help text everywhere
              tab-always-indent 'complete            ; Indent first then try completions
              uniquify-buffer-name-style 'forward    ; Uniquify buffer names
              use-dialog-box nil                     ; Disable dialog windows
              use-short-answers t                    ; Replace yes/no prompts with y/n
              vc-follow-symlinks t                   ; Silence warnings for following symlinked files
              ;; Disable backup files
              backup-inhibited t
              make-backup-files nil
              ;; Suppress non-critical warnings
              native-comp-async-report-warnings-errors 'silent
              warning-minimum-level :emergency
              ;; Tabs config
              c-basic-offset 2
              indent-tabs-mode nil
              tab-width 2)

;;;-----------------------------------------------------------------------------
;;; UI Enhancements
;;;-----------------------------------------------------------------------------

(blink-cursor-mode 0)                   ; Prefer a still cursor
(display-time-mode 1)                   ; Enable clock
(menu-bar-mode -1)                      ; Disable menu bar
(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar

;;;-----------------------------------------------------------------------------
;;; Buffer and Window Management
;;;-----------------------------------------------------------------------------

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)))

;;;-----------------------------------------------------------------------------
;;; Text Editing Enhancements
;;;-----------------------------------------------------------------------------

;; Enable `downcase-region'
(put 'downcase-region 'disabled nil)
;; Enable `upcase-region'
(put 'upcase-region 'disabled nil)
;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;-----------------------------------------------------------------------------
;;; File and Encoding Settings
;;;-----------------------------------------------------------------------------

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding

;;;-----------------------------------------------------------------------------
;;; File and Directory Management
;;;-----------------------------------------------------------------------------

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      package-user-dir (expand-file-name "elpa/" user-emacs-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      native-comp-eln-load-path (cons (expand-file-name "eln-cache/" user-emacs-directory)
                                      (cdr native-comp-eln-load-path)))

;;;-----------------------------------------------------------------------------
;;; Custom Functions
;;;-----------------------------------------------------------------------------

;; Open files with sudo
(defun sudo-find-file (file-name)
  "Open FILE-NAME as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(global-set-key (kbd "C-x C-r") 'sudo-find-file)

;; Delete buffer and file
(defun delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)
