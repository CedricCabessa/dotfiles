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

;; case insensitive
(setq case-fold-search t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq vc-follow-symlinks t)

(setq select-enable-primary t)

(setq recentf-max-saved-items 1000)


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
(bind-key "C-x b" 'helm-mini)
(use-package bufler
  :ensure t
  :diminish
  :bind
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

;; all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  )
(use-package all-the-icons-dired
  :ensure t
  :defer
  :hook ((dired-mode . all-the-icons-dired-mode))
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

(use-package python-pytest
  :ensure t
  )

(use-package with-venv
  :ensure t
  ) ; needed for dap-python

(use-package dap-mode
  :ensure t
  :config
  (setq dap-python-debugger 'debugpy)
  (require 'dap-mode)
  (require 'dap-python)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
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

(use-package lsp-metals ; scala
  :ensure t
  )

(use-package typescript-mode
  :ensure t
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

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  )

(use-package magit
  :ensure t
  :init
  (setq git-commit-max-length 50)
  )
(use-package forge
  :after magit
  :ensure t
  )

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t)
  )

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :init (require 'restclient-jq)
  :config
  (setq restclient-inhibit-cookies t)
  )

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind (:map copilot-completion-map
	      ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
	      )
  :hook ((prog-mode . copilot-mode))
  )

(use-package org
  :ensure t
  :init
  (setq org-duration-format (quote ((special . h:mm))))
  (setq org-log-into-drawer t)
  (setq org-todo-keywords (quote ((sequence "TODO(!)" "DONE(!)"))))
  (setq org-startup-folded t)
  (setq org-hide-emphasis-markers t)
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.1)
		  ))
    (set-face-attribute (car face) nil :height (cdr face)))
  (add-hook 'org-mode-hook 'my-org)
  ;; display 7 days in overview
  (setq org-agenda-ndays 7)
  ;; start calendar on the current day
  (setq org-agenda-start-on-weekday nil)
  ;; first day of week is monday
  (setq calendar-week-start-day 1)
  (setq org-capture-templates
   '(("i" "inbox" entry
      (file "~/org/inbox.org")
      "* %U %k\12%?" :prepend t)))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

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
     (restclient . t)
     ))
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (setq org-toggle-inline-images t)
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s %b")
	(todo . " %i %-12:c%?-12t% s %b")
	(tags . " %i %-12:c%?-12t% s %b")
	(search . " %i %-12:c%?-12t% s %b"))
	)
  :bind
  ("C-c a" . 'org-agenda)
  )

(defun my-org()
  (org-indent-mode 1)
)

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/org/roam/")
)

(use-package org-roam-ui
  :ensure t
  :after org-roam
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
  (deft-directory "~/org/roam")
  )

(use-package org-super-agenda
  :ensure t
  )


(use-package markdown-mode
  :ensure t
  :mode ("*\\.md" . gfm-mode)
  :config
  (setq markdown-css-paths '("https://markdowncss.github.io/retro/css/retro.css"))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)
  )
;; render ghm page
(use-package grip-mode
  :ensure t
  )

(use-package visual-fill-column
  :ensure t
  )
(use-package org-present
  :ensure t
  :init
  (add-hook 'org-present-mode-hook 'ced/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'ced/org-present-end)
  )

;; incompatible org version??

;; (use-package org-excalidraw
;;   :straight (:type git :host github :repo "wdavew/org-excalidraw")
;;   :config
;;   (setq org-excalidraw-directory "~/org/excalidraw")
;;   (org-excalidraw-initialize)
;; )
(setq org-excalidraw-directory "~/org/excalidraw")
(load-file "~/workdir/elisp/org-excalidraw.el")
(org-excalidraw-initialize)

;; text
(add-hook 'text-mode-hook 'my-text)
;(add-hook 'text-mode-hook 'texfrag-global-mode)
;; reload iimage on every change excalidraw?
;; https://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun my-text()
  (setq-default fill-column 120)
  (flyspell-mode 1)
  (auto-fill-mode 1)
  (iimage-mode 1)
  (setq-default visual-fill-column-width 120)
  (setq-default visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
)
(load-file "~/workdir/dotfiles/emacs/ced.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((pyvenv-workon . vault-api-gateway-k6r7IIeG)))
 '(org-agenda-files
   '("/home/ccabessa/org/perso/ion.org" "/home/ccabessa/org/perso/jdr.org" "/home/ccabessa/org/perso/perso.org" "/home/ccabessa/org/perso/ref.org" "/home/ccabessa/org/work/clock.org" "/home/ccabessa/org/work/interviews.org" "/home/ccabessa/org/work/ledger.org" "/home/ccabessa/org/inbox.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(vertico marginalia orderless bufler doom-modeline doom-themes all-the-icons all-the-icons-dired eglot pyvenv blacken rustic consult company company-quickhelp hl-todo magit forge git-link org org-roam-ui org-super-links helm-org-rifle deft org-super-agenda markdown-mode grip-mode visual-fill-column ruff-format restclient python-pytest projectile org-present yaml-mode restclient-jq jq-mode lsp-metals typescript-mode copilot with-venv pocket-reader ob-restclient))
 '(safe-local-variable-values '((pyvenv-workon . ledger-vault-api-AAYNituf)))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
