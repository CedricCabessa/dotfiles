(defun ced/pyvenv-show()
  "show current python venv"
    (interactive)
    (print pyvenv-virtual-env-name)
    )

(defun ced/last_download()
  (interactive)
  (find-file
   (car
    (seq-find
     '(lambda (x) (not (nth 1 x)))
     (sort
      (directory-files-and-attributes "~/Downloads" t nil t)
      '(lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))
    ))
  )


; https://stackoverflow.com/a/3669629
(defun ced/copy-path ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (message (file-truename buffer-file-name))
    )
  )

(defvar ced/addr-list '
  (("external tbtc"    . "tb1q4280xax2lt0u5a5s9hd4easuvzalm8v9ege9ge")
   ("sepolia"          . "0xA1C3389b3ab5525C16F90bFa6CAe7A7671d7Fc95")
   ("xrp saltqa"       . "r9AWXPzopvkL9Mq63cjzLvqVpr3KipaPLm")
   ("stellar"          . "GCH572NV4VU5QRXVEBN26MJOAQAYSA2H3TCWIEP7NVXN2ARIDJECQM47") ; saltqa
   ("tezos"            . "tz2Q7SZFTVz2K5mtDzB7HPrsJ6boEyBiUAo2") ; publicapi
   ("tron"             . "TPJZnKbZJaaVhtEzhDtP2PcytfAa743pFr") ; publicapi
   ))

(defun ced/addr ()
  "copy a blockchain addr of your choice in the kill ring"
  (interactive)
  (kill-new
   (message
    (cdr
     (assoc
      (completing-read
       "choose addr" (mapcar (lambda (x) (car x)) ced/addr-list)
       ) ced/addr-list
	 ))))
  )

(defun ced/uuid ()
  "copy a new uuid in the kill ring"
  (interactive)
  (let ((uuid (org-id-uuid)))
    (kill-new (message uuid))
    (insert uuid)
    uuid)
  )

(defun ced/org-jump ()
  "jump to a org headline"
  (interactive)
  (org-ql-search
    (org-agenda-files)
    (list 'and (list 'heading (read-string "headline: ")) '(level 1))
    :buffer "ced/org-jump")
  (with-current-buffer "ced/org-jump"
    (execute-kbd-macro (kbd "RET"))
    (org-narrow-to-subtree)
    (org-show-children)
    )
  )

(defun ced/org-find-files (path)
  (seq-filter
   (lambda (x) (and (not (string-match-p (regexp-quote "/archive/") x))
		    (not (string-match-p (regexp-quote "/roam/") x))
		    (not (string-match-p (regexp-quote "/.#") x))))
   (directory-files-recursively path "org$")
   )
  )


(setq math-additional-units
      '((btc nil "bitcoin")
	(bit "10^-6 * btc" "bit")
	(sat "10^-8 * btc" "satoshi")

	(eth nil "ether")
	(gwei "10^-9 * eth" "Gwei")
	(wei "10^-18 * eth" "Wei")
	)
      )
(setq math-units-table nil)


(defun vrb-get (var &rest path)
  (apply 'verb-json-get (oref (verb-stored-response var) body) path)
  )

(defun ced/verb-header-addtime (x)
  (concat x " | " (format-time-string "%T"))
  )

(defun ced/elfeed-browse-url-at-point-firefox (option)
  (interactive "p")
  (let ((url (or (elfeed-get-link-at-point)
                 (elfeed-get-url-at-point))))
    (if url
	(progn (message "%s" url)
	       (if (not (eq option 4))
		   (browse-url-firefox url)))
      (error "No URL found")
      )
    )
  )

(defun ced/lang-fr ()
  (interactive)
  (ispell-change-dictionary "francais")
  (setq flycheck-languagetool-language "fr-FR")
  )
(defun ced/lang-us ()
  (interactive)
  (ispell-change-dictionary "english")
  (setq flycheck-languagetool-language "en-US")
  )

(defvar ced/elfeed-score-sort-save nil)

(defun ced/elfeed-score-sort-random (a b)
  (= (random 2) 0))

(defun ced/elfeed-random (option)
  "Sort randomly rss feed"
  (interactive "p")
  (if (not ced/elfeed-score-sort-save)
      (fset 'ced/elfeed-score-sort-save (symbol-function 'elfeed-score-sort))
    )
  (if (eq option 1)
      (fset 'elfeed-score-sort (symbol-function 'ced/elfeed-score-sort-save))
    (fset 'elfeed-score-sort (symbol-function 'ced/elfeed-score-sort-random))
    )
  )



(defvar ced/clock-timer)
(defun ced/clock-start ()
  (interactive)
  (set 'clock-timer (run-with-timer 1 1
    (lambda ()
      (message (format-time-string "%Y-%m-%d %H:%M:%S"))
      )
    ))
  )

(defun ced/clock-stop ()
  (interactive)
  (cancel-timer clock-timer)
  (setq clock-timer nil)
  )


;; https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))


(defun ced/add-property-after-refile ()
  (let* ((current-file (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (when (and (org-at-heading-p)
	       (string-equal current-file "pocket.org"))
      (org-entry-put (point) "ARCHIVED" (format-time-string "%Y-%m-%d")))
    )
)
(add-hook 'org-after-refile-insert-hook 'ced/add-property-after-refile)
