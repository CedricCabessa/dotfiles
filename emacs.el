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
(add-to-list 'package-archives '("melpa" . "http://melpa-stable.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (other . "k&r"))))
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(eshell-save-history-on-exit t)
 '(fci-rule-color "gray14")
 '(fci-rule-column 80)
 '(fill-column 80)
 '(flyspell-default-dictionary "english")
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/org/perso/" "~/org/work/")))
 '(org-capture-templates
   (quote
    (("n" "notes" entry
      (file "~/org/notes.org")
      "* %T"))))
 '(org-mobile-directory "~/org/mobile")
 '(org-mobile-force-id-on-agenda-items nil)
 '(org-mobile-inbox-for-pull "~/org/mob.org")
 '(package-selected-packages
   (quote
    (magit fill-column-indicator markdown-mode exec-path-from-shell go-autocomplete elpy go-mode rust-mode)))
 '(show-paren-mode t nil (paren)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site"))
;; exec-path-from-shell bug due to "cannot set terminal process group (-1): Inappropriate ioctl for device"
;; setting the path here doesn't seem to works, so you need to symlink from
;; .local/bin
;;(exec-path-from-shell-initialize)

(setenv "SSH_AUTH_SOCK"
 (concat "/run/user/" (number-to-string (user-uid)) "/ssh-agent.socket"))

;;;;;;;;;;
;; VIEW ;;
;;;;;;;;;;

;; do not blink
(blink-cursor-mode -1)
;; who need a tool bar ?
(if (display-graphic-p) (tool-bar-mode 0))
;; show column number
(require 'fill-column-indicator)
(fci-mode)
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
(define-key global-map "\C-c." 'org-time-stamp)
;; display 7 days in overview
(setq org-agenda-ndays 7)
;; start calendar on the current day
(setq org-agenda-start-on-weekday nil)
;; first day of week is monday
(setq calendar-week-start-day 1)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

;;;;;;;;;;;;;;
;; SHORTCUTS;;
;;;;;;;;;;;;;;

;; compile
(global-set-key [f5] 'compile)
;; goto line
(global-set-key "\C-g" 'goto-line)
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

;;;;;;;;;;;;
;; CODE   ;;
;;;;;;;;;;;;

;; Qt hack
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))


(defun my-code ()
  (flyspell-prog-mode)
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
                                 1 font-lock-warning-face t)))
  (which-function-mode)
  (yas-minor-mode)
  (auto-complete-mode)
  (elpy-enable)
)

(defun my-text()
  (flyspell-mode)
)

(ac-config-default)
(require 'auto-complete-config)
;; go get github.com/rogpeppe/godef
;; go get github.com/nsf/gocode
(require 'go-autocomplete)

;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python3")
;; make sure needed package are installed: M-x elpy-config RET

(global-magit-file-mode)

(add-hook 'prog-mode-hook 'my-code)
(add-hook 'text-mode-hook 'my-text)

;;;;;;;;;;;;
;; NOTMUCH;;
;;;;;;;;;;;;

;; notmuch conf is split in another file
(if (file-exists-p "~/.notmuch.el") (load-file (expand-file-name "~/.notmuch.el")))
(setq user-mail-address "ced@ryick.net")
;; close sent message frame after sending
(setq message-kill-buffer-on-exit t)

;;last line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "light green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "green"))))
 '(font-lock-warning-face ((t (:background "orange" :foreground "black" :weight bold))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "blue")))))

