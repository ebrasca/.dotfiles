(setq-default ad-redefinition-action 'accept         ; Silence warnings for redefinition
              auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
              cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
              cursor-type '(hbar . 2)                ; Underline-shaped cursor
              custom-file "/dev/null"                ; Disable customize-*
              fill-column 80                         ; Set width for automatic line breaks
              frame-resize-pixelwise t               ; Disable frame
              gc-cons-threshold (* 8 1024 1024)      ; We're not using Game Boys anymore
              help-window-select t                   ; Focus new help windows when opened
              indent-tabs-mode nil                   ; Stop using tabs to indent
              inhibit-startup-screen t               ; Disable start-up screen
              initial-scratch-message ""             ; Empty the initial *scratch* buffer
              initial-major-mode #'org-mode          ; Prefer `org-mode' for *scratch*
              large-file-warning-threshold nil       ; Silence warnings for large files
              mouse-yank-at-point t                  ; Yank at point rather than pointer
              read-process-output-max (* 1024 1024)  ; Increase read size for data chunks
              show-help-function nil                 ; Disable help text everywhere
              tab-always-indent 'complete            ; Indent first then try completions
              uniquify-buffer-name-style 'forward    ; Uniquify buffer names
              vc-follow-symlinks nil                 ; Silence warnings for following symlinked files
              use-dialog-box nil                     ; To disable dialog windows
              use-short-answers t                    ; Replace yes/no prompts with y/n
              ;; Disable backup files
              make-backup-files nil
              backup-inhibited t
              native-comp-async-report-warnings-errors 'silent
              warning-minimum-level :emergency)
(blink-cursor-mode 0)                   ; Prefer a still cursor
(put 'downcase-region 'disabled nil)    ; Enable `downcase-region'
(put 'upcase-region 'disabled nil)      ; Enable `upcase-region'
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(load-theme 'misterioso)                ; Load theme
(menu-bar-mode -1)                      ; Disable menu bar
(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)))

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;;;-----------------------------------------------------------------------------
;;; Open files with sudo
;;;-----------------------------------------------------------------------------

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(global-set-key (kbd "C-x C-r") 'sudo-find-file)

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
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

;;;-----------------------------------------------------------------------------
;;; Set repos
;;;-----------------------------------------------------------------------------

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities '(("gnu-elpa" . 2)
                                   ("melpa" . 1)))

(package-initialize)

;;;-----------------------------------------------------------------------------
;;; Load use-package
;;;-----------------------------------------------------------------------------

(use-package use-package
  :custom (use-package-always-ensure t))

;;;-----------------------------------------------------------------------------
;;; Load packages for specific features and modes
;;;-----------------------------------------------------------------------------

(use-package flyspell)

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after all-the-icons
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :custom (doom-modeline-mode t))

(use-package no-littering)

(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 4))

(use-package pinentry
  :hook (after-init . pinentry-start))

(use-package vterm
  :custom
  (vterm-tramp-shells '(("docker" "/bin/zsh")))
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 1000))

(use-package multi-vterm
  :bind (:map global-map
              ("C-x c c" . multi-vterm)
              ("C-x c n" . multi-vterm-next)
              ("C-x c p" . multi-vterm-prev)
              ("C-x c ." . multi-vterm-project)))

(use-package focus)

(use-package beacon
  :config (beacon-mode 1))

(use-package which-key
  :config (which-key-mode)
  :custom (which-key-idle-delay 0.5))

(use-package marginalia
  :config (marginalia-mode))

(use-package hotfuzz
  :custom (completion-styles '(hotfuzz)))

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :ensure nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package magit
  :config
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-modules-overview
   'magit-insert-merge-log)
  :custom
  (epg-pinentry-mode 'loopback)
  (git-commit-fill-column 72)
  (magit-delete-by-moving-to-trash nil)
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face))
  (magit-diff-refine-hunk 'all)
  (magit-module-sections-nested nil)
  (magit-no-confirm '(stage-all-changes unstage-all-changes))
  (magit-section-initial-visibility-alist
   '((modules . show) (stashes . show) (unpulled . show) (unpushed . show))))

(use-package magit-todos
  :config (magit-todos-mode))

(use-package magit-org-todos
  :config (magit-org-todos-autoinsert))

(use-package column-enforce-mode
  :config (set-face-attribute 'column-enforce-face nil :foreground "#ff0000")
  :hook (prog-mode . column-enforce-mode))

(use-package nginx-mode)

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (slime-repl-mode . enable-paredit-mode)
         (sly-mode . enable-paredit-mode)))

(use-package slime
  :ensure nil
  :init (load "~/quicklisp/slime-helper.el")
  :custom
  (indent-tabs-mode nil)
  (slime-auto-select-connection 'always)
  (slime-kill-without-query-p t)
  (slime-load-failed-fasl 'never)
  :config
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "4GB")
                :coding-system utf-8-unix)))
  (slime-setup '(slime-mrepl
                 inferior-slime
                 slime-asdf
                 slime-fancy
                 slime-quicklisp))
  :hook
  (slime-mode . inferior-slime-mode))

(use-package erc
  :defer 3
  :delight "ε "
  :preface
  (require 'subr-x)
  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str
          (string-trim
           (replace-regexp-in-string "\n+" " " str))))
  :config
  (add-to-list 'erc-modules 'notifications)
  (erc-services-mode 1)
  (erc-update-modules)
  :custom
  (erc-autojoin-channels-alist '(("libera.chat"
                                  "#clim"
                                  "#nyxt"
                                  "#commonlisp"
                                  "#mezzano")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 13)
  (erc-header-line-format "%n on %t (%m)")
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server "irc.libera.chat")
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :hook
  ((ercn-notify . my/erc-notify)
   (erc-send-pre . my/erc-preprocess)))

(use-package gnus
  :custom
  (user-mail-address "ebrasca@librepanther.com")
  (user-full-name "Bruno Cichon")
  ;; imaps
  (gnus-select-method
   '(nnimap "librepanther"
     (nnimap-address "imap.librepanther.com")
     (nnimap-server-port 993)
     (nnimap-stream ssl)))
  ;; smtp
  (smtpmail-smtp-server "smtp.librepanther.com")
  (smtpmail-smtp-service 25)
  (smtpmail-stream-type 'starttls)
  (send-mail-function 'smtpmail-send-it)
  ;; sort functions
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date
     (not gnus-thread-sort-by-number)))
  ;; gui
  (gnus-permanently-visible-groups ".*")
  (gnus-summary-display-arrow t)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-ignore-subject t)
  ;; Archived Messages
  (gnus-message-archive-method
   '(nnfolder "archive"
     (nnfolder-inhibit-expiry t)
     (nnfolder-active-file "~/News/sent-mail/active")
     (nnfolder-directory "~/News/sent-mail/")))
  ;; Tree view for groups.
  (gnus-summary-line-format
   "%U%R%z %((%4,4k) %&user-date; %-32,32f %* %B%S%)\n")
  (gnus-user-date-format-alist '((t . "%d.%m.%y %H:%M")))
  (gnus-sum-thread-tree-indent          "  ")
  (gnus-sum-thread-tree-root            "● ")
  (gnus-sum-thread-tree-false-root      "◎ ")
  (gnus-sum-thread-tree-single-indent   "→ ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "└─► ")
  :hook
  (gnus-group-mode . gnus-topic-mode))

(use-package org
  :preface
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  :custom
  (org-clock-persist 'history)
  ;; log
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-reschedule 'logreschedule)
  (org-deadline-warning-days 30)
  (org-enforce-todo-dependencies t)
  (org-agenda-todo-ignore-with-date nil)
  (org-agenda-todo-ignore-deadlines nil)
  (org-agenda-todo-ignore-scheduled nil)
  (org-agenda-todo-ignore-timestamp nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  ;; org modules
  (org-modules '(org-habit))
  ;; Agenda Setup
  (org-directory "~/org")
  (org-default-notes-file "~/org/refile.org")
  (org-agenda-files '("~/org/todo.org"))
  ;; Task States
  (org-todo-keywords
   '((sequence "PROJECT(p)" "TODO(t)"
               "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
               "PHONE" "MEETING")))
  (org-todo-keyword-faces
   '(("PROJECT" :foreground "red" :weight bold)
     ("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)))
  ;; Capture templates
  (org-capture-templates
   '(("t" "todo" entry (file "~/org/refile.org")
      "* TODO %?\n" :clock-in t :clock-resume t)
     ("r" "respond" entry (file "~/org/refile.org")
      "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
      :clock-in t :clock-resume t :immediate-finish t)
     ("n" "note" entry (file "~/org/refile.org")
      "* %?" :clock-in t :clock-resume t)
     ("j" "Journal" entry (file+datetree "~/org/diary.org")
      "* %?\n" :clock-in t :clock-resume t)
     ("w" "org-protocol" entry (file "~/org/refile.org")
      "* TODO Review %c\n%U\n" :immediate-finish t)
     ("m" "Meeting" entry (file "~/org/refile.org")
      "* MEETING with %?" :clock-in t :clock-resume t)
     ("p" "Phone call" entry (file "~/org/refile.org")
      "* PHONE %?" :clock-in t :clock-resume t)
     ("h" "Habit" entry (file "~/org/refile.org")
      "* NEXT %?
  %U
  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
  :LOGBOOK:
  :END:
  :PROPERTIES:
  :STYLE: habit
  :REPEAT_TO_STATE: NEXT
  :END:")
     ("c" "Contacts" entry (file "~/org/contacts.org")
      "* Contact
  :PROPERTIES:
  :NAME:
  :EMAIL:
  :END:")))
  ;; Targets include this file and any file contributing to the agenda -
  ;; up to 9 levels deep
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  ;; Refile settings
  ;; Exclude DONE state tasks from refile targets
  (org-refile-target-verify-function 'bh/verify-refile-target)
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path t)
  ;; Targets complete directly with IDO
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; Use IDO for both buffer and file completion
  (org-completion-use-ido t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))
  (org-clock-persistence-insinuate)
  ;; Standard key bindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  ;; I use C-c c to start capture mode
  (global-set-key (kbd "C-c c") 'org-capture))

(use-package org-drill)
