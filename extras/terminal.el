;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Terminal
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Set default shell to fish if available
(let ((fish-path (executable-find "fish")))
  (setq shell-file-name
        (if fish-path fish-path
          (if (executable-find "zsh") (executable-find "zsh")
            (if (executable-find "bash") (executable-find "bash")
              (if (executable-find "bash") (executable-find "bash")
                (if (executable-find "pwsh") (executable-find "pwsh")
                  (executable-find "cmd"))))))))

;; ;; Eat: Emulate A Terminal
;; (use-package eat
;;   :custom
;;   (eat-term-name "xterm")
;;   :config
;;   (eat-eshell-mode)                     ; use Eat to handle term codes in program output
;;   (eat-eshell-visual-command-mode)      ; commands like less will be handled by Eat
;;   :bind (
;;          :map minimacs-term-keymap
;;          ("t" . eat)
;;          )
;;   )

(use-package vterm
  :ensure t
  :bind (:map minimacs-term-keymap
              ("t" . vterm))
  )
(defvar term-backend 'vterm)

;; Floating/Popup terminal
(use-package popterm
  :bind (("C-`"   . popterm-toggle)
         ("C-~"   . popterm-toggle-cd)
         ([f9]    . popterm-window-toggle))
  :config
  (setq popterm-backend        term-backend     ; or 'ghostel, 'eat, 'shell, 'eshell
        popterm-display-method 'window  ; or 'window, 'fullscreen
        popterm-scope          'project   ; or 'frame, 'dedicated, nil
        popterm-auto-cd        t)
  (popterm-global-mode 1))

;; Termint
(defmacro create-run-in-split (command-to-run)
  "Create an interactive function which runs COMMAND-TO-RUN in a vertical split."
  (let ((function-name (intern (format "%s-in-split" command-to-run) )))
    `(defun  ,function-name ()
       (interactive)
       (let ((repl-window (split-window-right)) (curr-window (selected-window)))
         (select-window repl-window)
         (,command-to-run)
         (select-window curr-window)))))

;; Python definition
(use-package termint
  :after python
  :bind (:map minimacs-repl-keymap
              ("s" . #'termint-ipython-start-in-split))
  :config
  (termint-define "ipython" #'termint-ipython-cmd-function
                  :bracketed-paste-p t
                  :source-syntax termint-ipython-source-syntax-template)
  ;; Define a function to run the REPL in a split window
  (create-run-in-split termint-ipython-start)
  ;; Set the terminal backend
  (setq termint-backend term-backend)
  ;; Setup the keymaps
  (keymap-set minimacs-repl-keymap "s" #'termint-ipython-start-in-split)
  (keymap-set minimacs-repl-keymap "r" #'termint-ipython-send-region)
  (keymap-set minimacs-repl-keymap "p" #'termint-ipython-send-paragraph)
  (keymap-set minimacs-repl-keymap "b" #'termint-ipython-send-buffer)
  (keymap-set minimacs-repl-keymap "f" #'termint-ipython-send-defun)
  (keymap-set minimacs-repl-keymap "R" #'termint-ipython-source-region)
  (keymap-set minimacs-repl-keymap "P" #'termint-ipython-source-paragraph)
  (keymap-set minimacs-repl-keymap "B" #'termint-ipython-source-buffer)
  (keymap-set minimacs-repl-keymap "F" #'termint-ipython-source-defun)
  (keymap-set minimacs-repl-keymap "h" #'termint-ipython-hide-window)
  )

;; R Definition
(use-package termint
  :after (ess-r-mode)
  :bind (:map minimacs-repl-keymap ("s" . termint-r-start-in-split))
  :config
  (termint-define "r" (executable-find "R")
                  :bracketed-paste-p t
                  :source-syntax "source(\"{{file}}\")")
  ;; Create function to run REPL in split
  (create-run-in-split termint-r-start)
  ;; Set terminal backend
  (setq termint-backend term-backend)

  ;; Setup the keymaps
  (keymap-set minimacs-repl-keymap "s" #'termint-r-start-in-split)
  (keymap-set minimacs-repl-keymap "e" #'termint-r-send-string)
  (keymap-set minimacs-repl-keymap "r" #'termint-r-send-region)
  (keymap-set minimacs-repl-keymap "p" #'termint-r-send-paragraph)
  (keymap-set minimacs-repl-keymap "b" #'termint-r-send-buffer)
  (keymap-set minimacs-repl-keymap "f" #'termint-r-send-defun)
  (keymap-set minimacs-repl-keymap "R" #'termint-r-source-region)
  (keymap-set minimacs-repl-keymap "P" #'termint-r-source-paragraph)
  (keymap-set minimacs-repl-keymap "B" #'termint-r-source-buffer)
  (keymap-set minimacs-repl-keymap "F" #'termint-r-source-defun)
  (keymap-set minimacs-repl-keymap "h" #'termint-r-hide-window)
  )



;; CIDER (clojure REPL)
(use-package cider
  :ensure t
  :commands (cider-jack-in))

(defun minimacs-clojure-mode-setup ()
  "Configure Clojure mode"
  (keymap-set minimacs-repl-keymap "s" #'cider-jack-in))
