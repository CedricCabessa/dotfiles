;; emacs.el
;; Author: ced@ryick.net - http://ced.ryick.net
;; License: WTFPL


;;            A programmer building an IDE with emacs
;;                is like a Jedi building his own lightsaber


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq use-package-always-ensure t)

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

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; UI / general
(blink-cursor-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq make-backup-files nil)
(setq visible-bell t)
(setq-default show-trailing-whitespace t)
(server-start)

;; search case insensitive
(setq case-fold-search t)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

; S-<left> S-<right> S-<up> S-<down> to switch window
(windmove-default-keybindings)

; vertical completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  )
; display info on completion (help, file metadata, etc)
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  )
; fuzzy matching on completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )
; buffer switch
(use-package bufler
  :ensure t
  :diminish
  ;:init (bufler-mode)
  :bind
  ("C-x b" . 'bufler-switch-buffer)
  ("C-x C-b" . 'bufler)
  )


; fancy modeline see M-x use nerd-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1)
  )

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )


(use-package eglot
  :ensure t
  :defer t
  :hook ((prog-mode . eglot-ensure))
  )

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; change venv when changing buffer
  (setq pyvenv-tracking-mode t)
  ;; don't show venv in modeline
  (setq pyvenv-mode-line-indicator nil)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode)
  )
(defun ced/pyvenv-show()
  "show current python venv"
    (interactive)
    (print pyvenv-virtual-env-name)
    )

(use-package blacken
  :ensure t
  :defer t
  :hook (python-mode . blacken-mode)
  )

(use-package rustic
  :ensure t
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package consult
  :ensure t
  :bind (("M-s r" . consult-ripgrep))
)

;; Provides completion, with the proper backend
;; it will provide Python completion.
(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; show popup completion on point
(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  )


(use-package magit
  :ensure t
  :init
  (setq git-commit-max-length 50)
  )

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t)
  )



(use-package org
  :ensure t
  :init
  (setq org-duration-format (quote ((special . h:mm))))
  (setq org-log-into-drawer t)
  (setq org-todo-keywords (quote ((sequence "TODO(!)" "DONE(!)"))))
  (setq org-startup-folded t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  ;; display 7 days in overview
  (setq org-agenda-ndays 7)
  ;; start calendar on the current day
  (setq org-agenda-start-on-weekday nil)
  ;; first day of week is monday
  (setq calendar-week-start-day 1)
  (setq org-capture-templates
	(quote
	 (("j" "Journal" entry
	   (file "~/org/journal.org")
	   "* %u %?
%F
" :prepend t)
     ("w" "work achievement" entry
      (file "~/org/work/journal.org")
      "* %u %?
" :prepend t)
     ("i" "inbox" entry
      (file "~/org/inbox.org")
      "* TODO %?\n %U
" :prepend t)
     )))
  (setq org-agenda-files
	(seq-filter
	 (lambda (x) (and (not (string-match-p (regexp-quote "/archive/") x))
			  (not (string-match-p (regexp-quote "/roam/") x))
			  (not (string-match-p (regexp-quote "/.#") x))))
	 (directory-files-recursively "~/org" "org$")
	 )
	)
  (setq org-agenda-custom-commands
  '(("c" . "My Custom Agendas")
    ("cu" "Unscheduled TODO"
     ((todo ""
       ((org-agenda-overriding-header "\nUnscheduled TODO")
        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))
       ))
     nil
     nil)
    )
  )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     ))
  (setq org-confirm-babel-evaluate nil)
  :bind
  ("C-c a" . 'org-agenda)
  )

(use-package org-roam-ui
  :ensure t
  )

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :ensure t
  :config
  (setq org-super-links-backlink-prefix nil)
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link))
  )
; needed by org-super-links to choose link
(use-package helm-org-rifle
  :ensure t
  )

(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :init
  (setq deft-use-filename-as-title t)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  )

(use-package org-super-agenda
  :ensure t
  )


(use-package markdown-mode
  :ensure t
  :mode ("*\\.md" . gfm-mode)
  :config
  (setq markdown-css-paths '("https://markdowncss.github.io/retro/css/retro.css"))
  )
;; render ghm page
(use-package grip-mode
  :ensure t
  )

;; text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult company-quickhelp company-quickhelp-mode company vertico orderless marginalia doom-themes doom-modeline))
 '(safe-local-variable-values '((pyvenv-workon . ledger-vault-api-AAYNituf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
