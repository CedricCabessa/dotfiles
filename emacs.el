;; emacs.el
;; Author: ced@ryick.net - http://ced.ryick.net
;; License: WTFPL


;;            A programmer building an IDE with emacs
;;                is like a Jedi building his own lightsaber



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (other . "k&r"))))
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (wheatgrass)))
 '(eshell-save-history-on-exit t)
 '(fci-rule-color "gray14")
 '(fci-rule-column 80)
 '(fill-column 80)
 '(flyspell-default-dictionary "english")
 '(git-commit-summary-max-length 50)
 '(git-link-open-in-browser t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(lsp-pyls-configuration-sources ["flake8"])
 '(lsp-pyls-plugins-flake8-enabled t)
 '(lsp-pyls-plugins-pycodestyle-enabled nil)
 '(lsp-pyls-plugins-pyflakes-enabled nil)
 '(markdown-command "pandoc --quiet --standalone")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "tracking" :query "thread:{tag:tracking} and thread:{tag:unread}")
     (:name "sentry" :query "tag:sentry and tag:unread"))))
 '(org-agenda-files
   (quote
    ("~/org/work/ledger.org" "~/org/journal.org" "~/org/mob.org")))
 '(org-babel-python-command "python3")
 '(org-capture-templates
   (quote
    (("j" "Journal" entry
      (file "~/org/journal.org")
      "* %u %?
%F
" :prepend t)
     ("w" "work achievement" entry
      (file "~/org/work/journal.org")
      "* %u %?
" :prepend t))))
 '(org-duration-format (quote ((special . h:mm))))
 '(package-selected-packages
   (quote
    (org blacken flycheck-mypy use-package lsp-ui lsp-mode deadgrep git-link flycheck notmuch racer editorconfig yaml-mode magit fill-column-indicator markdown-mode exec-path-from-shell go-autocomplete elpy go-mode rust-mode)))
 '(rust-format-on-save t)
 '(show-paren-mode t nil (paren)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site"))
;; exec-path-from-shell bug due to "cannot set terminal process group (-1): Inappropriate ioctl for device"
;; setting the path here doesn't seem to works, so you need to symlink from
;; .local/bin
;;(exec-path-from-shell-initialize)

(setenv "SSH_AUTH_SOCK"
 (concat "/run/user/" (number-to-string (user-uid)) "/keyring/ssh"))
(setenv "WORKON_HOME" "/home/ccabessa/.local/share/virtualenvs/")

;;;;;;;;;;
;; VIEW ;;
;;;;;;;;;;

;; do not blink
(blink-cursor-mode -1)
;; who need a tool bar ?
(if (display-graphic-p) (tool-bar-mode 0))
;; show column number
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq column-number-mode t)
;; two windows
(if (display-graphic-p) (split-window-vertically 20))


;;;;;;;;;;;;;;;;
;; BASIC CONF ;;
;;;;;;;;;;;;;;;;

;; do not pollute my folders with file~
(setq make-backup-files nil)
;; unicode is cool !
(set-language-environment "UTF-8")
;; default spell checker
(setq-default ispell-program-name "aspell")
;; mouse wheel on
(if (display-graphic-p) (mouse-wheel-mode 1))
;; visible bell on
(setq visible-bell 't)
;; show whitespace a the end of line
(setq-default show-trailing-whitespace t)
;; display image
(auto-image-file-mode)
;; start emacs server
(server-start)
;; no more "still has clients" prompt
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; ido
(ido-mode)

;;;;;;;;;;;;
;;ORG-MODE;;
;;;;;;;;;;;;

;; show agenda
(define-key global-map "\C-ca" 'org-agenda)
;; display 7 days in overview
(setq org-agenda-ndays 7)
;; start calendar on the current day
(setq org-agenda-start-on-weekday nil)
;; first day of week is monday
(setq calendar-week-start-day 1)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
))

;;;;;;;;;;;;;;
;; SHORTCUTS;;
;;;;;;;;;;;;;;

;; compile
(global-set-key [f5] 'compile)
;; goto matching parenthese
(global-set-key "%" 'match-paren)
;; move in kill ring in reverse
(global-set-key "\C-\M-y" 'yank-pop-neg)


;;;;;;;;;;;;
;; HELPERS;;
;;;;;;;;;;;;

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))


(defun yank-pop-neg (arg)
 	(interactive "p")
	(yank-pop -1)
)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d"))
)

;;;;;;;;;;;;
;; CODE   ;;
;;;;;;;;;;;;

;; Qt hack
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))


(defun my-code ()
  (flyspell-prog-mode)
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):"
                                 1 font-lock-warning-face t)))
  (which-function-mode)
  (auto-complete-mode)
)

(defun my-text()
  (flyspell-mode)
  (auto-fill-mode)
)

(ac-config-default)
(require 'auto-complete-config)
;; go get github.com/rogpeppe/godef
;; go get github.com/nsf/gocode
;; (require 'go-autocomplete)

(global-magit-file-mode)

(add-hook 'prog-mode-hook 'my-code)
(add-hook 'text-mode-hook 'my-text)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(require 'lsp-mode)
(use-package lsp-ui)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook 'blacken-mode)

(add-hook 'rust-mode-hook #'lsp)

(setq company-tooltip-align-annotations t)

;;;;;;;;;;;;
;; NOTMUCH;;
;;;;;;;;;;;;

;; notmuch conf is split in another file
;(load-file "/usr/share/emacs/site-lisp/notmuch.elc")
(if (file-exists-p "~/.notmuch.el") (load-file (expand-file-name "~/.notmuch.el")))
(setq user-mail-address "ced@ryick.net")
;; close sent message frame after sending
(setq message-kill-buffer-on-exit t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "ADBO" :family "Source Code Pro")))))
