;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors (easier than Meow's beacon)
(use-package multiple-cursors
  :ensure t
  :config (define-key mc/keymap (kbd "<return>") nil)
  :commands (
             mc/mark-next-like-this
	         mc/mark-previous-like-this
	         mc/edit-lines)
  )

;; Turn on hideshow in prog-mode
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; Better Undo
(use-package vundo
  :ensure t
  :bind (:map minimacs-utils-keymap
              ("u" . vundo))
  )

;; Remove Whitespace
(use-package ws-butler
  :ensure t
  :defer t
  :hook (prog-mode . ws-butler-mode))

;; Expand selected region
(use-package expand-region
  :ensure t
  :commands (er/expand-region
             er/contract-region)
  )

;; Change inside/outside current region
(use-package change-inner
  :ensure t
  :bind ("M-i" . change-inner)
  ("M-o" . change-outer))

;; Surround
(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))

;; Move lines or region
(use-package move-text
  :ensure t
  :bind (
	     ("M-<down>" . move-text-down)
	     ("M-<up>" .  move-text-up)
	     )
  )

;; Various useful functions
(use-package crux
  :ensure t
  :bind (
	     ("C-k" . crux-smart-kill-line)
         :map minimacs-utils-keymap
	     ("d" . crux-duplicate-current-line-or-region)
	     ("j" . crux-top-join-line)
         :map minimacs-windows-keymap
	     ("t" . crux-transpose-windows)
         :map minimacs-buffer-keymap
	     ("R" . crux-rename-file-and-buffer)
	     ("o" . crux-kill-other-buffers)
	     )
  )

;; Various built in settings
(use-package emacs
  :config
  ;; Code folding config
  (setopt hs-show-indicators t)         ; Show collapse indicators in margin
  (setopt hs-display-lines-hidden t)    ; Show number of collapsed lines
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (java-mode . java-ts-mode))
        )


  ;; Treesitter config

  ;; Enable tree-sitter in all available modes
  (setopt treesit-enabled-modes t)

  ;; Amount to highlight: integer between 1-4; 4 is max highlighting
  (setopt treesit-font-lock-level 3)

  ;; What to do if language grammar not installed: default is `ask';
  ;; other options are `always', and `ask-dir'.
  (setopt treesit-auto-install-grammar 'ask)

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;
;;;  Project   ;;
;;;;;;;;;;;;;;;;;
;; Project management
(use-package project
  :custom
  (when (>= emacs-major-version 30)
    (project-mode-line t)))         ; show project name in modeline


;;;;;;;;;;;;;;;;;
;;;  Linting   ;;
;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(r-lintr)) ;; lintr is VERY slow
  :config
  (global-flycheck-mode)
  (flycheck-pos-tip-mode)
  (global-flycheck-eglot-mode 1)
  :bind (
         :map minimacs-errors-keymap
         ("n" . flycheck-next-error )
         ("p" . flycheck-previous-error )
         ("e" . flycheck-explain-error-at-point )
         ("l" . flycheck-list-errors )
         ("s" . flycheck-select-checker )
         ("x" . flycheck-buffer )
         ("v" . flycheck-verify-setup )
         )
  )

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck)

;;;;;;;;;;;;;;;;;;;;
;;;  Formatting   ;;
;;;;;;;;;;;;;;;;;;;;
(use-package format-all
  :ensure t
  :commands (format-all-mode format-all-buffer)
  :hook (prog-mode . format-all-mode)
  :bind (:map minimacs-buffer-keymap ("f" . format-all-buffer))
  :config
  (setq-default format-all-formatters
		        '(
		          ("Shell" (shfmt "-i" "4" "-ci"))
		          ("Markdown" (mdformat))
		          )
                )
  )
