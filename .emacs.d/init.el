;;;-----------------------------------------------------------------------------
;;; Package Management
;;;-----------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;;;-----------------------------------------------------------------------------
;;; Org-Mode and Productivity
;;;-----------------------------------------------------------------------------

;; Org mode
(use-package org
  :custom
  ;; Prefer `org-mode' for *scratch*
  (initial-major-mode #'org-mode)
  ;; Clocking Work Time
  (org-clock-persist 'history)
  ;; Log
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
  ;; Org modules
  (org-modules '(org-habit))
  ;; Agenda Setup
  (org-directory "~/org")
  (org-default-notes-file "~/org/refile.org")
  (org-agenda-files '("~/org/todo.org"))
  ;; Task States
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "DONE(d)")
     (sequence "WAITING(w)" "HOLD(h)" "CANCELLED(c)"
               "PHONE(p)" "MEETING(m)")))
  (org-todo-keyword-faces
   '(("TODO"      :foreground "red"          :weight bold)
     ("NEXT"      :foreground "blue"         :weight bold)
     ("DONE"      :foreground "forest green" :weight bold)
     ("WAITING"   :foreground "orange"       :weight bold)
     ("HOLD"      :foreground "magenta"      :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("PHONE"     :foreground "forest green" :weight bold)
     ("MEETING"   :foreground "forest green" :weight bold)))
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
  (org-refile-targets '((nil              :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path t)
  ;; Targets complete directly with IDO
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; Use IDO for both buffer and file completion
  (org-completion-use-ido t)
  :config
  ;; Clocking Work Time
  (org-clock-persistence-insinuate)
  ;; Standard key bindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  ;; I use C-c c to start capture mode
  (global-set-key (kbd "C-c c") 'org-capture))

;; Spaced repetition learning
(use-package org-drill
  :after org)

;;;-----------------------------------------------------------------------------
;;; UI and Visual Enhancements
;;;-----------------------------------------------------------------------------

;; Themes
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :custom (doom-modeline-mode t))

;; Icons
(use-package nerd-icons)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Cursor highlighting when switching buffers
(use-package beacon
  :config (beacon-mode t))

;; Dim inactive text)
(use-package focus)

;; Distraction-free writing mode
(use-package writeroom-mode)

;;;-----------------------------------------------------------------------------
;;; File and Directory Management
;;;-----------------------------------------------------------------------------

;; Enhanced Dired
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(nerd-icons file-size file-time git-msg file-modes vc-state subtree-state))
  ;;(dirvish-attributes '(vc-state subtree-state nerd-icons git-msg file-modes file-time file-size))
  (dirvish-default-layout '(0 0.4 0.6))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort file-time file-size symlink) :right (omit yank index)))
  (dirvish-mode-line-height 10)
  (dirvish-mode-line-bar-image-width 0)
  (dirvish-path-separators (list (format " %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format " %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-use-header-line 'global)
  (delete-by-moving-to-trash nil)
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode))

;;;-----------------------------------------------------------------------------
;;; Terminal and Shell Integration
;;;-----------------------------------------------------------------------------

;; Terminal emulation
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

;;;-----------------------------------------------------------------------------
;;; Completion and Minibuffer Enhancements
;;;-----------------------------------------------------------------------------

;; Minibuffer completion
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
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :straight nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Minibuffer annotations
(use-package marginalia
  :config (marginalia-mode))

;; Fuzzy completion
(use-package hotfuzz
  :custom (completion-styles '(hotfuzz)))

;; Keybinding suggestions
(use-package which-key
  :config (which-key-mode)
  :custom (which-key-idle-delay 0.5))

(use-package free-keys)

;;;-----------------------------------------------------------------------------
;;; Version Control
;;;-----------------------------------------------------------------------------

;; Shows Git diff indicators in the margin
(use-package git-gutter
  :config (global-git-gutter-mode t))

;; A comprehensive Git interface
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

;; Integrates TODO items with Magit
(use-package magit-todos
  :config (magit-todos-mode))

;; Integrates Org-mode TODOs with Magit
(use-package magit-org-todos
  :config (magit-org-todos-autoinsert))

;; Integrates GitHub/GitLab with Magit
(use-package forge
  :after magit)

;;;-----------------------------------------------------------------------------
;;; Programming and Development
;;;-----------------------------------------------------------------------------

;; Structured editing for Lisp
(use-package paredit
  :hook ((emacs-lisp-mode                  . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (ielm-mode                        . paredit-mode)
         (lisp-interaction-mode            . paredit-mode)
         (lisp-mode                        . paredit-mode)
         (scheme-mode                      . paredit-mode)
         (slime-repl-mode                  . paredit-mode)
         (sly-mode                         . paredit-mode)))

;; Common Lisp IDE
(use-package slime
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

;; Guile Scheme IDE
(use-package geiser-guile)

;; Guix package manager
(use-package guix)

;; Nginx mode
(use-package nginx-mode)

;; Column enforcement
(use-package column-enforce-mode
  :config (set-face-attribute 'column-enforce-face nil :foreground "#ff0000")
  :hook (prog-mode . column-enforce-mode))

;;;-----------------------------------------------------------------------------
;;; Communication
;;;-----------------------------------------------------------------------------

;; IRC client
(use-package erc
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
  (erc-action-face    ((t (:foreground "#8fbcbb"))))
  (erc-error-face     ((t (:foreground "#bf616a"))))
  (erc-input-face     ((t (:foreground "#ebcb8b"))))
  (erc-notice-face    ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :hook
  ((ercn-notify . my/erc-notify)
   (erc-send-pre . my/erc-preprocess)))

;; Email and news reader
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

;;;-----------------------------------------------------------------------------
;;; Miscellaneous
;;;-----------------------------------------------------------------------------

;; Spell checking
(use-package flyspell)

;; Handles GPG passphrase entry
(use-package pinentry
  :hook (after-init . pinentry-start))

;; Undo tree
(use-package vundo
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(use-package esup
  :custom
  (esup-depth 0))

(use-package pass)
