;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (:map minimacs-vcs-keymap
         ("g" . magit-status)))

;; Git time-machine (step through git history)
(use-package git-timemachine
             :commands (git-timemachine)
             :bind (:map minimacs-vcs-keymap
                    ("t" . git-timemachine)))

;; Git signs in gutter 
(use-package git-gutter
             :ensure t
             :hook (prog-mode . git-gutter-mode)
             :config (git-gutter:linum-setup)
             :bind (:map minimacs-vcs-keymap
                    ("n" . git-gutter:next-hunk)
                    ("p" . git-gutter:previous-hunk)
                    ("P" . git-gutter:popup-hunk)
                    ("s" . git-gutter:stage-hunk)
                    ("r" . git-gutter:revert-hunk)))
