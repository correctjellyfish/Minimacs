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

;; Update the cursor shape inside a terminal; e.g. when in insert mode
;; when using Evil (Vim emulation) change the cursor to a bar.
(setopt xterm-update-cursor t)

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

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width
(setq display-line-numbers 'relative) ; Use relative numbers

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

(setopt global-hl-line-sticky-flag 'window) ; Every window gets own hl-line instance
(global-hl-line-mode)

;; Use this to enable the line highlight in only certain modes:
;(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
;  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Show matching delimiters
(setopt show-paren-delay 0)
(setopt show-paren-mode t)
(setopt show-paren-style 'expression)   ; default is 'parenthesis and just does delimiters
(setopt show-paren-context-when-offscreen 'overlay)

;; Theme
(use-package emacs
  :config
  (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi
