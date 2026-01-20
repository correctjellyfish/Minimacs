;; -*- lexical-binding: t; -*-
;;__/\\\\____________/\\\\_________________________________________________________________________________________
;; _\/\\\\\\________/\\\\\\_________________________________________________________________________________________
;;  _\/\\\//\\\____/\\\//\\\__/\\\________________/\\\_______________________________________________________________
;;   _\/\\\\///\\\/\\\/_\/\\\_\///___/\\/\\\\\\___\///_____/\\\\\__/\\\\\____/\\\\\\\\\________/\\\\\\\\__/\\\\\\\\\\_
;;    _\/\\\__\///\\\/___\/\\\__/\\\_\/\\\////\\\___/\\\__/\\\///\\\\\///\\\_\////////\\\_____/\\\//////__\/\\\//////__
;;     _\/\\\____\///_____\/\\\_\/\\\_\/\\\__\//\\\_\/\\\_\/\\\_\//\\\__\/\\\___/\\\\\\\\\\___/\\\_________\/\\\\\\\\\\_
;;      _\/\\\_____________\/\\\_\/\\\_\/\\\___\/\\\_\/\\\_\/\\\__\/\\\__\/\\\__/\\\/////\\\__\//\\\________\////////\\\_
;;       _\/\\\_____________\/\\\_\/\\\_\/\\\___\/\\\_\/\\\_\/\\\__\/\\\__\/\\\_\//\\\\\\\\/\\__\///\\\\\\\\__/\\\\\\\\\\_
;;        _\///______________\///__\///__\///____\///__\///__\///___\///___\///___\////////\//_____\////////__\//////////__

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic Setup
;;;  - Completions
;;;  - Customization
;;;  - Editing Enhancements
;;;  - UI
;;;  - Theme
;;;  - Developement
;;;    - Linting
;;;    - LSP
;;;    - Debugging
;;;    - Formatting
;;;    - Language Modes
;;;    - Templating
;;;    - Version Control
;;;  - REPL/Terminal
;;;  - Org Mode
;;;  - Writing
;;;  - Keymaps

;;; Guardrail

(when (< emacs-major-version 29)
  ("Config only works with Emacs 29 and newer; you have version %s" emacs-major-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Initialization (including MELPA)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; If you want to turn off the welcome screen, uncomment this
(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;; The above creates nested directories in the backup folder. If
;; instead you would like all backup files in a flat structure, albeit
;; with their full paths concatenated into a filename, then you can
;; use the following configuration:
;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
;;
;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
;;   (setopt backup-directory-alist `(("." . ,backup-dir))))

;; Set scroll margin
(setq scroll-margin 2)
(setq scroll-conservatively 101)

;; Replace selection when typing (or pasting)
(delete-selection-mode 1)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
;; Eagerly Loaded
(use-package which-key
  :ensure t
  :config (which-key-mode))


;; Minibuffers and Completions
(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager


(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Set font if not in terminal (Useful to set nerd font)
(defun font-exists-p (font) (if (null (x-list-fonts font)) nil t))
(when (window-system)
  (cond ((font-exists-p "Fira Code Nerd Font Mono") (set-frame-font "Fira Code Nerd Font Mono:spacing=100:size=14" nil t))
	((font-exists-p "Courier New") (set-frame-font "Courier New:spacing=100:size=18" nil t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Customization
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes '(default))
 '(package-selected-packages
   '(beacon cape catppuccin-theme centaur-tabs change-inner company
	    corfu-terminal crux dap-mode dashboard eat eglot elsa
	    embark-consult envrc ess flycheck-pos-tip format-all
	    general json-mode kind-icon lsp-ui magit marginalia
	    move-text multiple-cursors orderless rust-mode smartparens
	    tempel-collection termint tuareg typst-ts-mode undo-fu
	    vertico wgrep writeroom-mode ws-butler yaml-mode))
 '(package-vc-selected-packages
   '((typst-ts-mode :url
		    "https://codeberg.org/meow_king/typst-ts-mode.git")))
 '(truncate-lines t)
 '(writeroom-global-effects
   '(writeroom-set-fullscreen writeroom-set-alpha
			      writeroom-set-menu-bar-lines
			      writeroom-set-tool-bar-lines
			      writeroom-set-vertical-scroll-bars
			      writeroom-set-bottom-divider-width
			      my/writeroom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Completions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
;; Eager
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :ensure nil ;; Included in vertico
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t corfu-auto-delay 0.1 corfu-quit-no-match 'separator)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))


;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Editing Enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak the zap character
(keymap-global-set "M-z" 'zap-up-to-char)

;; allows for jumping around visual field
(use-package avy
  :ensure t
  :bind (
         ("C-c j j" . avy-goto-word-0)
         ("C-c j l" . avy-goto-line)
         ("C-c j c" . avy-goto-char-timer)
         ("C-c j w" . avy-goto-word-1)
         )
  )

;; Easy jumping between windows
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  :bind(
        ("C-c n" . mc/mark-next-like-this-word)
        ("C-c N" . mc/skip-to-next-like-this)
        ("C-c p" . mc/mark-previous-like-this-word)
        ("C-c P" . mc/skip-to-previous-like-this)
        ("C-c m j" . mc/mark-next-like-this )
        ("C-c m k" . mc/mark-next-like-this )
        ("C-c m ^" . mc/edit-beginnings-of-lines )
        ("C-c m $" . mc/edit-ends-of-lines )
        ("C-c m s" . mc/mark-all-in-region )
        ("C-c m v" . mc/vertical-align )
        ("C-c m f" . mc/mark-all-like-this-in-defun )
        ("C-c m i a" . mc/insert-letters )
        ("C-c m i n" . mc/insert-letters )
        )
  )

;; Better Undo
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  :bind
  ("C-c u u" . undo-fu-only-undo)
  ("C-c u r" . undo-fu-only-redo)
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo)
  )


;; Remove Whitespace
(use-package ws-butler
  :ensure t
  :defer t
  :hook (prog-mode . ws-butler-mode))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Leader key functions
         ("C-c f r" . consult-recent-file)
         ("C-c f f" . consult-fd)
         ("C-c f g" . consult-ripgrep)
         ("C-c f l" . consult-line)
         ("C-c f i" . consult-imenu)
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

(use-package embark-consult
  :ensure t
  :after consult)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))


;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Expand selected region
(use-package expand-region
  :ensure t
  :bind (
	 ("C-=" . er/expand-region)
	 ("C-c =" . er/expand-region)
	 )
  )

;; Change inside/outside current region
(use-package change-inner
  :ensure t
  :bind
  ("C-c C-i" . change-inner)
  ("C-c C-o" . change-outer)
  )

;; Smart Parens
(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  :bind
  ("C-c s s" . sp-forward-slurp-sexp)
  ("C-c s S" . sp-backward-slurp-sexp)
  ("C-c s b" . sp-forward-barf-sexp)
  ("C-c s B" . sp-backward-barf-sexp)
  ("C-c s n" . sp-next-sexp)
  ("C-c s p" . sp-previous-sexp)
  ("C-c s r" . sp-rewrap-sexp)
  ("C-c s d" . sp-unwrap-sexp)
  ("C-c s D" . sp-backward-unwrap-sexp)
  ("C-c s c" . sp-change-enclosing)
  ("C-c s (" . sp-wrap-round)
  ("C-c s {" . sp-wrap-curly)
  ("C-c s [" . sp-wrap-square)
  )

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
	 ("C-c o" . crux-smart-open-line)
	 ("C-c O" . crux-smart-open-line-above)
	 ("C-c u d" . crux-duplicate-current-line-or-region)
	 ("C-c u j" . crux-join-top-line)
	 ("C-c w t" . crux-transpose-windows)
	 ("C-c b R" . crux-rename-file-and-buffer)
	 ("C-c b o" . crux-kill-other-buffers)
	 ("C-c b b" . consult-buffer )
	 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; For terminal users, make the mouse more useful
(xterm-mouse-mode 1)

(setq display-line-numbers-type 'relative)      ; Use relative line numbers
(setopt display-line-numbers-width 2)           ; Set a minimum width
(global-display-line-numbers-mode)              ; Show line numbers

;; Line Truncation/Continuation

(add-hook 'text-mode-hook 'visual-line-mode)

;; Help tracking cursor
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

;; Set modes to highlight current line in
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; File tree
(use-package treemacs
  :ensure t
  :commands treemacs
  :bind ("C-c f t" . treemacs))

;; Welcome Screen
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'modus-vivendi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Developement
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config
  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))


(use-package project
  :custom
  (when (>= emacs-major-version 30)
    (project-mode-line t)))         ; show project name in modeline

;; Direnv integration
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

;;;;;;;;;;;;;;;;;
;;;  Linting   ;;
;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(r-lintr)) ;; lintr is VERY slow
  (global-flycheck-mode)
  (flycheck-pos-tip-mode)
  )

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck)

;;;;;;;;;;;;;
;;;  LSP   ;;
;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (;; Start LSP when in a "programming" file
         (python-ts-mode . lsp)
         (python-mode . lsp)
         (cc-mode . lsp)
         ;; which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind ("C-c l l" . lsp)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp
  )

;; Integrate LSP with Treemacs
(use-package lsp-treemacs
  :ensure t
  :config (lsp-treemacs-sync-mode 1)
  :commands lsp)

;;;;;;;;;;;;;;;;;;
;;;  Debugging  ;;
;;;;;;;;;;;;;;;;;;

(use-package dap-mode
  :ensure t
  :config (add-hook 'dap-stopped-hook
		    (lambda (arg) (call-interactively #'dap-hydra)))
  :bind (
	 ("C-c d b" . dap-breakpoint-toggle)
	 ("C-c d s" . dap-debug)
	 ("C-c d o" . dap-go-to-output-buffer)
	 ("C-c d c" . dap-continue)
	 ("C-c d n" . dap-next)
	 ("C-c d i" . dap-step-in)
	 ("C-c d u" . dap-step-out)
	 ("C-c d e" . dap-eval-thing-at-point)
	 ("C-c d g" . dap-eval-region)
	 ("C-c d r" . dap-restart-frame)
         )
  )



;; Native Debugging
(use-package dap-gdb
  :after dap-mode)

;; Python Debugging
(use-package dap-python
  :after dap-mode
  :config (setq dap-python-debugger 'debugpy)
  :mode ("\\.py\\'" . python-ts-mode)
  )

;;;;;;;;;;;;;;;;;;;;
;;;   Formatting  ;;
;;;;;;;;;;;;;;;;;;;;

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
		'(
		  ("Shell" (shfmt "-i" "4" "-ci"))
		  )
		)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Language Modes  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  )

;; Typst
(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode.git")
  :mode "\\.typ\\'"
  )

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

(use-package tuareg
  :defer t
  :ensure t
  :mode ("\\.ml\\'"
	 "\\.mli\\'"
	 "\\.mlp\\'")
  )

;;;;;;;;;;;;;;;;;;;
;;;  Templating  ;;
;;;;;;;;;;;;;;;;;;;

(use-package tempel
  :ensure t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Collection of useful templates
(use-package tempel-collection
  :ensure t
  :after tempel
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Version Control  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c g" . magit-status)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Terminal/REPL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the default shell to Fish
(setq shell-file-name "/usr/bin/fish")

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode)     ; commands like less will be handled by Eat
  :bind (
         ("C-c t t" . eat)
         )
  :commands eat
  )

;; Termint
(use-package termint
  :ensure t
  :after python
  :bind (:map python-ts-mode-map ("C-c r s" . termint-ipython-start))
  :config
  (termint-define "ipython" "ipython" :bracketed-paste-p t
                  :source-syntax termint-ipython-source-syntax-template)
  (setq termint-backend 'eat)

  ;; C-c r s: `termint-ipython-start'
  ;; C-c r e: `termint-ipython-send-string'
  ;; C-c r r: `termint-ipython-send-region' (or `termint-ipython-send-region-operator' if evil is installed.)
  ;; C-c r p: `termint-ipython-send-paragraph'
  ;; C-c r b: `termint-ipython-send-buffer'
  ;; C-c r f: `termint-ipython-send-defun'
  ;; C-c r R: `termint-ipython-source-region' (or `termint-ipython-source-region-operator' if evil is installed.)
  ;; C-c r P: `termint-ipython-source-paragraph'
  ;; C-c r B: `termint-ipython-source-buffer'
  ;; C-c r F: `termint-ipython-source-defun'
  ;; C-c r h: `termint-ipython-hide-window'
  (define-key python-ts-mode-map (kbd "C-c r") termint-ipython-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org-Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Writing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable flyspell prog mode when activating prog-mode
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Enable flyspell mode for text mode
(add-hook 'text-mode-hook #'flyspell-mode)

(defun my/writeroom (arg)
  "Hook used for writeroom-mode"
  (cond
   ((= arg 1)
    (progn
      (setq display-line-numbers nil)
      (visual-line-mode)
      )
    )
   ((= arg -1)
    (progn
      (setq display-line-numbers t)
      (visual-line-mode)
      )
    )
   )
  )

(use-package visual-fill-column
  :defer t
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :bind (
         ("C-c z" . writeroom-mode)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Keymaps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;;;  Hydras   ;;
;;;;;;;;;;;;;;;;

(defhydra hydra-flycheck ()
  "Flycheck"
  ("n" flycheck-next-error "Next Error")
  ("p" flycheck-previous-error "Previous Error")
  ("e" flycheck-explain-error-at-point "Explain")
  ("l" flycheck-list-errors "List errors")
  ("s" flycheck-select-checker "Select checker")
  ("x" flycheck-buffer "Flycheck buffer")
  ("v" flycheck-verify-setup "Verify setup")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  General Keybinds   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control)

;; Avy default bindings
(avy-setup-default)

(use-package general
  :config
  (general-create-definer start/leader-keys
    :keymaps 'override
    :prefix "C-c"
    :global-prefix "C-c")

  ;; Buffer keymaps
  (start/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    )

  ;; Comment keymaps
  (start/leader-keys
    "c" '(:ignore t :wk "comment")
    "c l" '(comment-line :wk "line")
    "c r" '(comment-or-uncomment-region :wk "region")
    "c d" '(comment-dwim :wk "dwim")
    )

  ;; Debugging
  (start/leader-keys
    "d" '(:ignore t :wk "debug")
    )

  ;; File/Find Keymaps
  (start/leader-keys
    "f" '(:ignore t :wk "file/find")
    "f d" '(dired :wk "Open dired")
    "f j" '(dired-jump :wk "Dired jump to current")
    "f w" '(write-file :wk "Write File (with name)")
    "f s" '(save-buffer :wk "Save Buffer")
    "f S" '(save-some-buffer :wk "Save Some Buffer")
    )

  ;; Git
  (start/leader-keys
    "g" '(:ignore t :wk "git")
    )

  ;; Jump
  (start/leader-keys
    "j" '(:ignore t :wk "jump")
    )

  ;; Language keymaps (lsp-mode, etc.)
  (start/leader-keys
    "l" '(:ignore t :wk "language")
    )

  ;; Multicursor
  (start/leader-keys
    "m" '(:ignore wk "multicursor")
    "m i" '(:ignore :wk "insert")
    )

  ;; Open line (Above or below)
  (start/leader-keys
    "o" '(:ignore t :wk "Line below")
    "O" '(:ignore t :wk "Line above")
    )

  ;; REPL (Termint)
  (start/leader-keys
    "r" '(:ignore t :wk "REPL")
    )

  ;; SEXP
  (start/leader-keys
    "s" '(:ignore t :wk "Sexp/Parens")
    )

  ;; Toggle/Terminal
  (start/leader-keys
    "t" '(:ignore t :wk "terminal/toggle")
    "t t" '(:ignore t :wk "Terminal")
    "t w" '(visual-line-mode :wk "Toggle line wrap")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    )

  ;; Utility functions (mostly crux)
  (start/leader-keys
    "u" '(:ignore t :wk "utils")
    "u d" '(:ignore t :wk "Duplicate line/region")
    "u j" '(:ignore t :wk "Join line")
    )

  ;; Window keymaps
  (start/leader-keys
    "w" '(:ignore t :wk "window")
    "w h" '(windmove-left :wk "Left Window")
    "w j" '(windmove-down :wk "Down Window")
    "w k" '(windmove-up :wk "Up Window")
    "w l" '(windmove-right :wk "Right Window")
    "w q" '(delete-window :wk "Close Window")
    "w |" '(split-window-right :wk "Split Vertical")
    "w -" '(split-window-below :wk "Split Horizontal")
    "w t" '(:ignore t :wk "Transpose windows")
    )

  ;; Flycheck
  (start/leader-keys
    "!" '(:ignore t :wk "flycheck (default)")
    )
  (start/leader-keys
    "x" '(hydra-flycheck/body :wk "flycheck")
    )

  ;; Zen/Writing
  (start/leader-keys
    "z" '(:ignore t :wk "Zen/Writing")
    )
  )
