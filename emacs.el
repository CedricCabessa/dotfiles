;; emacs.el
;; Author: ced@ryick.net - http://ced.ryick.net
;; License: WTFPL


;;            A programmer building an IDE with emacs
;;                is like a Jedi building his own lightsaber

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


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
(setq visible-bell 't)
(setq-default show-trailing-whitespace t)
(server-start)


;; search case insensitive
(setq case-fold-search t)

;; parenthesis
(show-paren-mode 1)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; move in kill ring in reverse
(defun yank-pop-neg (arg)
  (interactive "p")
  (yank-pop -1)
)
(global-set-key "\C-\M-y" 'yank-pop-neg)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  )
(use-package orderless
  :ensure t
  :init (icomplete-mode)
  :custom (completion-styles '(orderless))
  )
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )
(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure)
)
; pylsp-mypy
; pylsp-rope
; python-lsp-ruff
; pyls-isort
; python-lsp-black

(use-package org
  :ensure t
  :init
  (setq org-duration-format (quote ((special . h:mm))))
  (setq org-log-into-drawer t)
  (setq org-todo-keywords (quote ((sequence "TODO(!)" "DONE(!)"))))
  (setq org-startup-folded t)
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


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/org/roam")
  (org-roam-db-autosync-mode)
)

(use-package org-roam-ui
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

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-color "gray14")
  (setq fci-rule-column 80)
  :config
  (fci-mode 1)
  )


(use-package magit
  :ensure t
  :init
  (setq git-commit-max-length 50)
)

(use-package git-link
  :ensure t
  :init
  (setq git-link-open-in-browser t)
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

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					  (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(use-package blacken
  :ensure t
  )
(add-hook 'python-mode-hook 'blacken-mode)
;;; /python

(use-package rustic
  :ensure t
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


(use-package plantuml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (setq plantuml-jar-path "~/.local/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-indent-level 2)
  )

(use-package fic-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fic-mode)
  )

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :init
;;   (setq emojify-emoji-styles '(unicode))
;;   )

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :ensure t
  :config
  (setq org-super-links-backlink-prefix nil)
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)))

;; text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; wheatgrass theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(wheatgrass))
 '(fill-column 80)
 '(js-indent-level 2)
 '(org-agenda-files
   '("~/org/perso/jdr.org" "~/org/perso/perso.org" "~/org/perso/ref.org" "~/org/work/clock.org" "~/org/work/interviews.org" "~/org/work/journal.org" "~/org/work/ledger.org" "~/org/journal.org"))
 '(org-agenda-prefix-format
   '((agenda . "%i %-12:c %?-12t %b")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-id-link-to-org-use-id t)
 '(package-selected-packages
   '(yaml projectile fzf rustic rust-mode dockerfile-mode elpher fic-mode org-roam-ui deft plantuml-mode go-mode emojify counsel yaml-mode docker org-super-agenda org-super-agenda-mode blacken pyvenv lsp-ui company lsp-pyright lsp-jedi magit use-package fill-column-indicator git-link org-roam protobuf-mode scala-mode ivy))
 '(read-file-name-completion-ignore-case t)
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fic-face ((t (:foreground "red" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.4)))))
