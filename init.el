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

;; Remap CapsLock to Ctrl
;; TODO need to add the remapping to be tangle
(start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

(defvar vinid/default-font-size 140)
(defvar vinid/default-variable-font-size 140)

(set-face-attribute 'default nil :font "Fira Code Retina" :height vinid/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height vinid/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height vinid/default-variable-font-size :weight 'regular)

(use-package all-the-icons)

(ido-mode 1)

(global-display-line-numbers-mode t)



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

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)

                                        ; to avoid creating infinite buffers with dired. I am not sure if this is working or not

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
       loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

                                        ;  (use-package dired-hide-dotfiles
                                        ;   :hook (dired-mode . dired-hide-dotfiles-mode)
                                        ;  :config
                                        ; (bind-key   "H" 'dired-hide-dotfiles-mode))

(defun vinid/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

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

(defun vinid/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . vinid/org-mode-visual-fill)) 

(use-package org
    :hook (org-mode . vinid/org-mode-setup)
    :config
    (setq org-ellipsis " ▾"))

  (setq org-log-done 'time)

  (setq org-log-into-drawer t)

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-adapt-indentation t)

(defun vinid/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(set-fringe-mode 0)

(setq org-agenda-start-with-log-mode t)

(setq orgroam-elisp-folder  "~/Dropbox/org/orgroam/")

(setq org-agenda-files '("~/Dropbox/org/orgmode/todos.org"))

;; Agenda View "d"
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-skip-deadline-if-done t)

(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-custom-commands
      '(
        ;; Daily Agenda & TODOs
        ("d" "Daily agenda and all TODOs"

         ;; Display items with priority A
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))

          ;; View 3 days in the calendar view
          (agenda "" ((org-agenda-span 3)))

          ;; Display items with priority B (really it is view all items minus A & C)
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                   (air-org-skip-subtree-if-priority ?C)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))

          ;; Display items with pirority C
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
          )

         ;; Don't compress things (change to suite your tastes)
         ((org-agenda-compact-blocks nil)))
        ))

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

(setq org-capture-templates
    '(("c" "TODO" entry (file+datetree "~/Dropbox/org/orgmode/inbox.org")
      "* TODO %?\n  %i")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/orgroam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

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

  (setq exwm-manage-force-tiling nil)

  ;; Automatically move EXWM buffer to current workspace when selected
(setq exwm-layout-show-all-buffers t)

;; Display all EXWM buffers in every workspace buffer list
(setq exwm-workspace-show-all-buffers t)

(use-package magit)

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
     "feh" nil  "feh --bg-scale /home/vinid/Pictures/wall.jpg"))

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
    (exwm-input-set-key (kbd "\C-c SPC") 'counsel-linux-app) 

    ;; (counsel-linux-app)
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
            ([?\C-h] . [left delete])
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


;; raise the specified app if it's already started, otherwise start it
;; this should ideally raise buffer the previous buffer, not the current one
;; meaning: if I had chrome on the right side and I call this from the left side
;;          it should show up on the right side

(defun vinid/run-or-raise (buffer-prefix &optional cmd)
      (let ((existing-buffer
                 (cl-dolist (buffer (buffer-list))
                       (if (string-prefix-p buffer-prefix (buffer-name buffer))
                               (cl-return buffer)))))
        (if existing-buffer
                ;; it's currently displayed, go to it
                (if (get-buffer-window existing-buffer)
                              (message (format "%s" (pop-to-buffer existing-buffer)))
                      (exwm-workspace-switch-to-buffer existing-buffer))
              (start-process-shell-command buffer-prefix nil cmd))))


  (defun goto-wm-logseq ()
      "raise 'logseq'"		
      (interactive)
      (vinid/run-or-raise "Logseq" "flatpak run com.logseq.Logseq"))


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

    (exwm-enable))

(require 'exwm-randr)

(exwm-randr-enable)

(setq exwm-randr-workspace-monitor-plist '(2 "DP-2"))

(setq exwm-workspace-warp-cursor t)


(defun vinid/update-displays ()
  (vinid/run-in-background "autorandr --change --force")
                                        ;    (message "Display config: %s"
  (string-trim (shell-command-to-string "autorandr --current"))))

(add-hook 'exwm-randr-screen-change-hook #'vinid/update-displays)
(vinid/update-displays)

(defun vinid/run-in-background (command)
   (let ((command-parts (split-string command "[ ]+")))
     (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(vinid/run-in-background "dropbox start")

(defun vinid/emacs-configuration ()
   (interactive)
   (find-file "~/.emacs.d/emacs_configuration.org"))

 (defun vinid/open-todolist ()
   (interactive)
   (find-file "~/Dropbox/org/orgmode/todos.org"))

(defun vinid/open-inbox ()
   (interactive)
   (find-file "~/Dropbox/org/orgmode/inbox.org"))

(setq clean-buffer-list-delay-special (* 1 3600))
(setq clean-buffer-list-delay-general 1)
(global-set-key (kbd "C-c e b") 'clean-buffer-list)

(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c e c") 'vinid/emacs-configuration)
(global-set-key (kbd "C-c e t") 'vinid/open-todolist)
(global-set-key (kbd "C-c e i") 'vinid/open-inbox)

(use-package 1passel
  :straight '(1passel :host github
                                 :repo "vinid/1passel"
                                 :branch "master"))

(defun gpt-improve()
  (interactive)
    (kill-new (shell-command-to-string
             (concat "python3 /home/vinid/nope/gptask.py --string '" (current-kill 0) "'"))))

  (global-set-key (kbd "C-c g") 'gpt-improve)

;  (straight-use-package   '(nano :type git :host github :repo "rougier/nano-emacs"))

(straight-use-package '(nano-theme :type git :host github   :repo "rougier/nano-theme"))

(load-theme 'nano t)
(nano-dark)
                                        ;  (nano-theme-set-dark)
                                        ; (nano-faces)
                                        ;(nano-theme)

(load-theme 'nano t)
(menu-bar-mode -1)
(tool-bar-mode -1)
