;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Movement 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (
         :map minimacs-jump-keymap
           ("j" . avy-goto-word-0)
           ("l" . avy-goto-line)
           ("c" . avy-goto-char-timer)
           ("w" . avy-goto-word-1)
         )
  )

;; Easy jumping between windows
(use-package ace-window
  :ensure t
  :bind ("M-j" . ace-window))
