;; org-mode
(require 'org-bullets)
(require 'org)
(require 'org-journal)

(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-log-done t)

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)


(setq org-agenda-files '("~/Dropbox/org/gtd/study.org"
                         "~/Dropbox/org/gtd/gtd.org"
			 "~/org/research.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/gtd/gtd.org" "Tasks")
                               "* TODO %i%?")))

(setq org-refile-targets
  '(("~/Dropbox/org/gtd/archive.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "DONE(d)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("PROG" :foreground "yellow" :weight bold)
     	      ("WAIT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)
