;; org-mode
(require 'org-bullets)
(require 'org)
(setq org-log-done t)

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)


(setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
                         "~/Dropbox/org/gtd/gtd.org"))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")))

(setq org-refile-targets
  '(("~/Dropbox/org/gtd/archive.org" :maxlevel . 1)))


;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold))))


(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)
