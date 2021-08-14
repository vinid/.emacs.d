(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)

(setq clean-buffer-list-delay-special (* 1 3600))
(setq clean-buffer-list-delay-general 1)
(global-set-key (kbd "C-c e b") 'clean-buffer-list)

(defvar vinid/default-font-size 140)
(defvar vinid/default-variable-font-size 140)

;; Make frame transparency overridable
(defvar vinid/frame-transparency '(90 . 90))

(set-face-attribute 'default nil :font "Fira Code Retina" :height vinid/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height vinid/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height vinid/default-variable-font-size :weight 'regular)

(use-package all-the-icons)

(use-package spacegray-theme :defer t)
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(ido-mode 1)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(global-display-line-numbers-mode t)

(set-frame-parameter (selected-frame) 'alpha vinid/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,vinid/frame-transparency))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun vinid/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . vinid/org-mode-visual-fill))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (bind-key   "H" 'dired-hide-dotfiles-mode))

(use-package ledger-mode
  :straight t 
  :mode ".ldg")

;;    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;	 (use-package mu4e
;	   :ensure nil
;	   :load-path "/usr/share/emacs/site-lisp/mu4e/"
;	   :defer 20 ; Wait until 20 seconds after startup
;	   :config

           ;; This is set to 't' to avoid mail syncing issues when using mbsync
;	   (setq mu4e-change-filenames-when-moving t)

           ;; Refresh mail using isync every 10 minutes
;	   (setq mu4e-update-interval (* 1000 60))
;	   (setq mu4e-get-mail-command "mbsync -a")
;	   (setq mu4e-maildir "~/Mail")

;	   (setq mu4e-drafts-folder "/[Gmail]/.Drafts")
;	   (setq mu4e-sent-folder   "/[Gmail]/.Sent Mail")
;	   (setq mu4e-refile-folder "/[Gmail]/.All Mail")
;	   (setq mu4e-trash-folder  "/[Gmail]/.Trash")


 ; (setq message-send-mail-function 'smtpmail-send-it)
        ; (setq mu4e-maildir-shortcuts
;	 '(("/Inbox"             . ?i)
;	   ("/[Gmail]/.Sent Mail" . ?s)
;	   ("/[Gmail]/.Trash"     . ?t)
;	   ("/[Gmail]/.Drafts"    . ?d)

;	   ("/[Gmail]/.All Mail"  . ?a))))


;  (setq mu4e-contexts
;	(list
         ;; Work account
;	 (make-mu4e-context
;	  :name "Work"
;	  :match-func
;	    (lambda (msg)
;	      (when msg
;		(string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
;	  :vars '((user-mail-address . "chiccobia@gmail.com")
;		  (user-full-name    . "Federico Bianchi")
;		  (smtpmail-smtp-server  . "smtp.gmail.com")
;		  (smtpmail-smtp-service . 465)
;		  (smtpmail-stream-type  . ssl)
;		  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
;(mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
 ;                 (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
  ;                (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))


  ; (setq epa-file-cache-passphrase-for-symmetric-encryption t)

 ;    (mu4e t)

(use-package perspective
:straight t  ; use `:straight t` if using straight.el!
:bind (("C-x k" . persp-kill-buffer*))
:init
(persp-mode))

(defun vinid/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . vinid/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;; making the eshell prompt starting with a lambda char
(setq eshell-prompt-function
         (lambda ()
            (concat "[" (getenv "USER") "]"
             (eshell/pwd) (if (= (user-uid) 0) " # " " λ "))))

(server-start)
  
(setq mouse-autoselect-window t
      focus-follows-mouse t)
  
  (defun vinid/exwm-init-hook ()
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1))
  
  
  (defun vinid/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))
  ;; defines a function that makes a nicer visualization for the firefox tab
  (defun vinid/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))
  
  
  (defun vinid/set-wallpaper ()
    (interactive)
    ;; NOTE: You will need to update this to a valid background path!
    (start-process-shell-command
     "feh" nil  "feh --bg-scale /home/vinid/Pictures/Wallpapers/forest.jpg"))
  
  (use-package exwm
    :config
    ;; Set the default number of workspaces
    (setq exwm-workspace-number 5)
  
    ;; When window "class" updates, use it to set the buffer name
    (add-hook 'exwm-update-class-hook #'vinid/exwm-update-class)
  
    ;; When EXWM starts up, do some extra configuration
    (add-hook 'exwm-init-hook #'vinid/exwm-init-hook)
  
    (setq mouse-autoselect-window nil
          focus-follows-mouse nil)
  
    ;; When window title updates, use it to set the buffer name
  
    (add-hook 'exwm-update-title-hook #'vinid/exwm-update-title)
    ;; To add a key binding only available in line-mode, simply define it in
    ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
  
    ;; adding a way to run apps
    (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  
    ;; toggle fullscreen
    (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
  
    ;; Set the wallpaper after changing the resolution
    (vinid/set-wallpaper)
  
    ;; These keys should always pass through to Emacs
    (setq exwm-input-prefix-keys
          '(?\C-x
            ?\C-u
            ?\C-h
            ?\M-x
            ?\M-`
            ?\M-&
            ?\M-:
            ?\C-\M-j  ;; Buffer list
            ?\C-\ ))  ;; Ctrl+Space
  
    ;; Ctrl+Q will enable the next key to be sent directly
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  
    ;; The following example demonstrates how to use simulation keys to mimic
    ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
    ;; and DEST is what EXWM actually sends to application.  Note that both SRC
    ;; and DEST should be key sequences (vector or string).
    (setq exwm-input-simulation-keys
          '(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\M-d] . [C-S-right delete])
            ([?\C-k] . [S-end delete])
            ;; cut paste
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ;; search
            ([?\C-s] . [?\C-f])))
  
    ;; Set up global key bindings.  These always work, no matter the input state!
    ;; Keep in mind that changing this list after EXWM initializes has no effect.
    (setq exwm-input-global-keys
          `(
            ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
            ([?\s-r] . exwm-reset)
  
            ;; Move between windows
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
  
            ;; Launch applications via shell command
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
  
            ;; Switch workspace
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
  
            ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))
  
    (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  
    (exwm-enable))

(require 'exwm-randr)

(exwm-randr-enable)

(setq exwm-randr-workspace-monitor-plist '(2 "HDMI-1-2"))

(setq exwm-workspace-warp-cursor t)


                                        ;  (defun vinid/update-displays ()
                                        ;    (vinid/run-in-background "autorandr --change --force")
                                        ;    (message "Display config: %s"
                                        ;             (string-trim (shell-command-to-string "autorandr --current"))))

                                        ;  (add-hook 'exwm-randr-screen-change-hook #'vinid/update-displays)
                                        ;  (vinid/update-displays)

(defun vinid/run-in-background (command)
   (let ((command-parts (split-string command "[ ]+")))
     (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(vinid/run-in-background "dropbox start")

(vinid/run-in-background "dunst")

(defun vinid/disable-desktop-notifications ()
  (interactive) 
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun vinid/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun vinid/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

(defvar vinid/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun vinid/kill-panel ()
  (interactive)
  (when vinid/polybar-process
    (ignore-errors
      (kill-process vinid/polybar-process)))
  (setq vinid/polybar-process nil))

(defun vinid/start-panel ()
  (interactive)
  (vinid/kill-panel)
  (setq vinid/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun vinid/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun vinid/send-polybar-exwm-workspace ()
  (vinid/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'vinid/send-polybar-exwm-workspace)
(vinid/start-panel)

(setq exwm-workspace-number 4)

(defun vinid/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

  (defun vinid/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

(set-fringe-mode 0)

(use-package org
  :hook (org-mode . vinid/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(setq org-agenda-start-with-log-mode t)

(setq org-log-done 'time)

 (setq org-log-into-drawer t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-journal)

(setq org-src-tab-acts-natively t)
(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-log-done t)
(setq org-journal-file-type 'weekly)


  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cl" 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)


  (setq org-agenda-files '(
                           "~/Dropbox/org/gtd/gtd.org"
                           "~/Dropbox/org/gtd/habits.org"))


  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/org/gtd/inbox.org" "Refiling")
                                 "* TODO %i%?")))

  (setq org-refile-targets
    '(("~/Dropbox/org/gtd/gtd.org" :maxlevel . 3)
      ("~/Dropbox/org/gtd/archive.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")))

(setq org-agenda-span 'day)


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("PROG" :foreground "yellow" :weight bold)
              ("WAIT" :foreground "blue" :weight bold)
              ("NEXT" :foreground "orange" :weight bold)
              ("INTR" :foreground "pink" :weight bold)
              ("DONE" :foreground "forest green" :weight bold))))

 (setq org-treat-S-cursor-todo-selection-as-state-change nil)

 (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-agenda-custom-commands
  '(("n" "Agenda / INTR / PROG / NEXT"
     ((agenda "" nil)
      (todo "INTR" nil)
      (todo "PROG" nil)
      (todo "NEXT" nil))
     nil)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(use-package org
   :config
  (add-to-list 'org-modules 'org-tempo))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically tangle our Emacs.org config file when we save it
(defun vinid/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs_configuration.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'vinid/org-babel-tangle-config)))

(use-package org-roam
      :straight t
      :custom
      (org-roam-directory (file-truename "/home/vinid/Dropbox/org/roam"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup))
      ;; If using org-roam-protocol
;      (require 'org-roam-protocol))

  (setq org-roam-v2-ack t)

  (add-to-list 'exec-path "/usr/bin/") ; probably not necessary

;  (add-hook 'after-init-hook 'org-roam-mode)

(use-package org-ref)

 (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(use-package magit)

(defun vinid/emacs-configuration ()
    (interactive)
    (find-file "~/.emacs.d/emacs_configuration.org"))

  (defun vinid/gtd-file ()
    (interactive)
    (find-file "~/Dropbox/org/gtd/gtd.org"))

(defun vinid/inbox-file ()
    (interactive)
    (find-file "~/Dropbox/org/gtd/inbox.org"))

  (global-set-key (kbd "C-c e c") 'vinid/emacs-configuration)
  (global-set-key (kbd "C-c e g") 'vinid/gtd-file)
  (global-set-key (kbd "C-c e r") 'vinid/inbox-file)

(global-set-key (kbd "C-ò") 'delete-backward-char)

(use-package 1passel
  :straight '(1passel :host github
                                 :repo "vinid/1passel"
                                 :branch "master"))
