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

(load-file "~/workdir/dotfiles/emacs/ced.el")

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

(global-auto-revert-mode 1)

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
  :bind
  ("C-c t" . 'python-pytest-dispatch)
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


(use-package ruff-format
  :ensure t
  :defer t
  :hook (python-mode . ruff-format-on-save-mode)
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

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  )

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
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  )
(use-package forge
  :after magit
  :ensure t
  )

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-default-branch "main")
  )

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1)
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
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
  (add-hook 'org-src-mode-hook (lambda () (setq-local flycheck-disabled-checkers '(languagetool))))
  ;; display 7 days in overview
  (setq org-agenda-ndays 7)
  ;; start calendar on the current day
  (setq org-agenda-start-on-weekday nil)
  ;; first day of week is monday
  (setq calendar-week-start-day 1)
  (setq org-capture-templates
   '(("i" "inbox" entry
      (file "~/org/inbox.org")
      "* %U %k%?" :prepend t)
     ("f" "future" entry
      (file "~/org/inbox.org")
      "* TODO %?\nSCHEDULED %^t\n" :prepend t)
     ("k" "knowledge" entry
      (file "~/org/knowledge.org")
      "* %?"  :prepend t)
     ("d" "decision" entry
      (file "~/org/decision.org")
      "* %U %? %^G" :prepend t)
     )
    )
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

  (setq org-agenda-files (ced/org-find-files "~/org"))
  (setq org-agenda-custom-commands
  '(("c" . "My Custom Agendas")
    ("cu" "Unscheduled TODO"
     ((todo ""
       ((org-agenda-overriding-header "\nUnscheduled TODO")
        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))
       ))
     nil
     nil)
    ("cU" "Untagged" ((tags-todo "-{.*}")))
    ("cw" "Work" agenda "" ((org-agenda-files (ced/org-find-files "~/org/work"))))
    ("cp" "Perso" agenda "" ((org-agenda-files (ced/org-find-files "~/org/perso"))))
    )
  )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
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
  (add-hook 'org-agenda-mode-hook (lambda () (org-agenda-to-appt 1)))
  (add-to-list 'org-modules 'org-habit t)
  (run-with-timer 5 3600 'org-agenda-to-appt 1)
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture)
  ("C-c !" . 'org-timestamp-inactive)
  )

(use-package appt
  :ensure nil ; built-in
  :init
  (appt-activate 1)
  :config
  (require 'notifications)
  (setq appt-disp-window-function
    (lambda (min-to-app new-time msg)
      (notifications-notify :title "emacs" :body msg :urgency 'normal)
      (appt-disp-window min-to-app new-time msg)
      )
    )
  (setq appt-message-warning-time 0)
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

(use-package visual-fill-column
  :ensure t
  )

(use-package dslide
  :ensure t
  :config
  (setq dslide-breadcrumb-separator " > ")
  )

(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode)
  :config
  (setq emojify-emoji-styles '(unicode))
  :bind (("C-x :" . emojify-insert-emoji))
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

(use-package yaml-pro
  :ensure t
  :init
  (add-hook 'yaml-mode-hook 'yaml-pro-ts-mode)
  )

;; text
(add-hook 'text-mode-hook 'my-text)
;(add-hook 'text-mode-hook 'texfrag-global-mode)
;; reload iimage on every change excalidraw?
;; https://emacs.stackexchange.com/questions/3302/live-refresh-of-inline-images-with-org-display-inline-images

(defun my-text()
  (setq-default fill-column 100)
  (flyspell-mode 1)
  (auto-fill-mode 1)
  (iimage-mode 1)
  (setq-default visual-fill-column-width 120)
  (setq-default visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (flymake-mode 1)
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package elfeed-protocol
  :ensure t
  :config
  (setq elfeed-protocol-enabled-protocols '(fever ttrss))
  (elfeed-protocol-enable)
  (setq elfeed-protocol-feeds '(("ttrss+https://cedc@ced.ryick.net/tt-rss"
				 :password "ZZZZ")))
  )

(use-package elfeed
  :ensure t
  :bind (:map elfeed-show-mode-map ("f" . ced/elfeed-browse-url-at-point-firefox))
  :config
  (setq elfeed-search-title-max-width 100)
  (add-hook 'elfeed-show-mode-hook (lambda (): (setq show-trailing-whitespace nil)))
  )

(use-package elfeed-score
  :ensure t
  :config
  (setq elfeed-score-serde-score-file "~/.elfeed/elfeed.score")
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  )

(use-package gptel
  :ensure t
  :init
  (setq gptel-model "gemini-1.5-flash"
        gptel-default-mode 'org-mode
	gptel-backend (gptel-make-gemini "Gemini" :key "ZZZZ" :stream t))
  :hook
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  )

(use-package verb
  :ensure t
  :init
  (advice-add 'verb--response-header-line-string :filter-return #'ced/verb-header-addtime)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (add-hook                       ; https://github.com/federicotdn/verb/issues/82
   'org-ctrl-c-ctrl-c-hook
   (lambda ()
     (when (and (member "verb" (org-get-tags))
                verb-mode)
       (call-interactively #'verb-send-request-on-point-other-window-stay))))
  )

(use-package pocket-reader
  :ensure t
  )


(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (add-hook 'eshell-mode-hook (lambda (): (setq show-trailing-whitespace nil)))
  )

(setq package-selected-packages
      '(
	all-the-icons
	all-the-icons-dired
	blacken
	bufler
	company
	company-box
	company-quickhelp
	consult
	consult-todo
	copilot
	deft
	doom-modeline
	doom-themes
	dslide
	eat
	eglot
	elfeed
	elfeed-protocol
	elfeed-score
	emojify
	flycheck-languagetool
	forge
	git-gutter
	git-link
	gptel
	helm-org-ql
	helm-org-rifle
	hl-todo
	jq-mode
	kubed
	lsp-metals
	magit
	marginalia
	markdown-mode
	nov
	orderless
	org
	org-present
	org-roam-ui
	org-super-agenda
	org-super-links
	pocket-reader
	python-pytest
	pyvenv
	ruff-format
	rustic
	typescript-mode
	verb
	vertico
	visual-fill-column
	with-venv
	yaml-mode
	yaml-pro
	)
      )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-always-kill t)
 '(flycheck-keymap-prefix "")
 '(package-vc-selected-packages '((aider :url "https://github.com/tninja/aider.el")))
 '(safe-local-variable-values
   '((pyvenv-workon . vault-api-gateway-k6r7IIeG) (pyvenv-workon . vault-tradelink-QDkmTEDo-3.13)
     (pyvenv-workon . ledger-vault-api-AAYNituf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
