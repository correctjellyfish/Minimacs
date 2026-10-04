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

;; Eat: Emulate A Terminal
(use-package eat
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode)      ; commands like less will be handled by Eat
  :bind (
         :map minimacs-term-keymap
         ("t" . eat)
         )
  )

;; Termint
(use-package termint
  :ensure t
  :after python
  :config
  (termint-define "ipython" "ipython" :bracketed-paste-p t
                  :source-syntax termint-ipython-source-syntax-template)
  (setq termint-backend 'eat)

  :commands (
             termint-ipython-start
             )
  )

(defun minimacs-python-ts-mode-setup ()
  "Configure python-ts-mode"
  (keymap-set minimacs-repl-keymap "s" #'termint-ipython-start)
  (keymap-set minimacs-repl-keymap "e" #'termint-ipython-send-string)
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
(add-hook 'python-ts-mode-hook 'minimacs-python-ts-mode-setup)

;; CIDER (clojure REPL)
(use-package cider
  :ensure t
  :commands (cider-jack-in))

(defun minimacs-clojure-mode-setup ()
  "Configure Clojure mode"
  (keymap-set minimacs-repl-keymap "s" #'cider-jack-in))
