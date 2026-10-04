;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((python-mode markdown-mode typst-ts-mode zig-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  ;; Avoid changing line heights if your font is wonky. See
  ;; https://github.com/joaotavora/eglot/discussions/1492
  (setopt eglot-code-action-indicator "h")

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("rass" "--" "marksman" "server" "--" "harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("rass" "--" "tinymist" "--" "harper-ls" "--stdio")))

  ;; You can set various options for each language server. For
  ;; example, you can raise the number of completions surfaced by a
  ;; given langauge server to Emacs:
  ; (setopt eglot-workspace-configuration
  ;  '((haskell (maxCompletions . 100))
  ;    (elixir  (maxCompletions . 100))))
  :bind (
        :map minimacs-language-keymap
        ("s" . eglot)
        ("r" . eglot-rename)
        ("a" . eglot-code-actions)
        ("f" . eglot-format-buffer)
        ("h" . eglot-inlay-hints-mode)
        ("d" . xref-find-definitions)
        ("R" . xref-find-references)
        ("i" . imenu)
         )
  )

(use-package eldoc-box
  :ensure t
  :config (eldoc-box-hover-at-point-mode)
  )

(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
