;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Search
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consult: Misc. enhanced commands
(use-package consult
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("C-y" . consult-yank-from-killring) ; orig. yank
         ("M-y" . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         :map minimacs-search-keymap
         ("g" . consult-ripgrep)
         ("l" . consult-line)       ; Alternative: rebind C-s to use
         ("s" . consult-line)       ; consult-line instead of isearch, bind
         ("L" . consult-line-multi) ; isearch to M-s s
         ("o" . consult-outline)
         ("e" . consult-compile-error)
         ;; Buffer
         :map minimacs-buffer-keymap
         ("b" . consult-buffer)
         ;; Files
         :map minimacs-file-keymap
         ("h" . consult-recent-file)
         ("s" . consult-fd)
         :map minimacs-utils-keymap
         ("y" . consult-yank-pop)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

;; Modify search results en mass
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Search region on Wikipedia
(defun search-region-wikipedia ()
  "Search the current region on Wikipedia."
  (interactive)
  (let (
        (search-url "https://en.wikipedia.org/wiki/Special:Search?search=")
        (region (buffer-substring (mark) (point)))
        )
    (browse-url (concat search-url region))
    )
  )

(keymap-set minimacs-search-keymap "w" 'search-region-wikipedia)

(provide 'search)
;;; search.el ends here
