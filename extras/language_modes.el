;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Modes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode)

(use-package json-mode)

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode))
  :mode ("\\.md\\'")
  )

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'"
	 "\\.yml\\'")
  )

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'"
	 "\\.jsonc\\'")
  )

;; CSV
(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'"
	 "\\.tsv\\'")
  )

;; Rust
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  )

;; Java
(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'")

;; Clojure
(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . lsp)
  :mode ("\\.clj\\'")
  )

;; Java Treesitter Mode
(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'"
  )

;; Go-ts-mode (built-in)
(use-package go-ts-mode
  :ensure nil ;; builtin
  :mode "\\.go\\'"
  :config (require 'dap-dlv-go))


;; Typst
(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :mode "\\.typ\\'"
  )

;; Meson
(use-package meson-mode
  :ensure t
  :mode "meson.build\\'")

;; Justfile
(use-package just-mode
  :ensure t
  :mode "justfile\\'")

;; R
(defun my/insert-R-pipe ()
  "Insert '|>' at point, moving point forward."
  (interactive)
  (insert "|>"))

(defun my/insert-R-assignment ()
  "Insert '<-' at point, moving point forward."
  (interactive)
  (insert "<-"))
;; use Air to format the content of the file
(defun run-air-on-r-save ()
  "Run Air after saving .R files and refresh buffer."
  (when (and (stringp buffer-file-name)
             (string-match "\\.R$" buffer-file-name))
    (let ((current-buffer (current-buffer)))
      (shell-command (concat "air format " buffer-file-name))
      ;; Refresh buffer from disk
      (with-current-buffer current-buffer
        (revert-buffer nil t t)))))
(use-package ess
  :ensure t
  :defer t
  :hook (after-save . run-air-on-r-save)
  :config
  (keymap-global-set "C-c >" #'my/insert-R-pipe)
  (keymap-global-set "C-c -" #'my/insert-R-assignment)

  )
(load "ess-autoloads")

;; Major mode for OCaml programming
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune
  :ensure t)

;; Merlin provides advanced IDE features
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

;; Zig programming language
(use-package zig-mode
  :ensure t
  :hook (zig-mode . lsp)
  :mode ("\\.zig\\'" "\\.zon\\'"))
