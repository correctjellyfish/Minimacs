;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   User Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(setopt mode-line-collapse-minor-modes nil)        ; nil default; set to `t' to hide minor modes

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Set scroll margin
(setq scroll-margin 2)
(setq scroll-conservatively 101)

;; Update the cursor shape inside a terminal; e.g. when in insert mode
;; when using Evil (Vim emulation) change the cursor to a bar.
(setopt xterm-update-cursor t)

;; Tab-line to show buffers as tabs
(global-tab-line-mode)

;; These are too personal to prescribe a default; uncomment and
;; configure according to your tastes
(setopt indent-tabs-mode nil) ; Only use spaces to perform indentation
(setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Makes it easier to repeat commands; `C-x o C-x o' becomes `C-x o o'
;; See https://karthinks.com/software/it-bears-repeating/
(repeat-mode)

;; Display line numbers
(setopt display-line-numbers-width 3)           ; Set a minimum width
(setq display-line-numbers-type 'relative) ; Use relative numbers
(global-display-line-numbers-mode)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

(setopt global-hl-line-sticky-flag 'window) ; Every window gets own hl-line instance
(global-hl-line-mode)

;; Help tracking cursor
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

;; Set modes to highlight current line in
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show matching delimiters
(setopt show-paren-delay 0)
(setopt show-paren-mode t)
(setopt show-paren-style 'expression)   ; default is 'parenthesis and just does delimiters
(setopt show-paren-context-when-offscreen 'overlay)

;; Theme
(use-package catppuccin-theme
  :ensure t
  :init (setq catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin t))


;; Set font if not in terminal (Useful to set nerd font)
(defun font-exists-p (font) (if (null (x-list-fonts font)) nil t))
(when (window-system)
  (cond ((font-exists-p "Fira Code Nerd Font Mono") (set-frame-font "Fira Code Nerd Font Mono:spacing=100" nil t))
	((font-exists-p "Courier New") (set-frame-font "Courier New:spacing=100" nil t))))

;; File tree
(use-package treemacs
  :ensure t
  :commands treemacs
  :bind (
         :map minimacs-file-keymap ("t" . treemacs))
  )
;; Get a random element of a list
(defun random-element-of-list (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))
(defvar logo-titles (list 
                      "Mea Navis Aëricumbens Anguillis Abundat" 
                      "Nolite te Bastardes Carborundorum"
                          )
  )

;; Welcome Screen
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title (random-element-of-list logo-titles))
  (setq dashboard-footer-messages (list (shell-command-to-string "fortune")))
  (setq dashboard-startup-banner (random-element-of-list (directory-files (expand-file-name "images/" user-emacs-directory) t "\\.txt")))
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

;; Indent guides
(use-package indent-bars
  :ensure t
  :hook ((python-ts-mode python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer
