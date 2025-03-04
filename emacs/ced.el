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
   ("ropsten"          . "0x687422eEA2cB73B5d3e242bA5456b782919AFc85")
   ("xrp saltqa"       . "r9AWXPzopvkL9Mq63cjzLvqVpr3KipaPLm")
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
