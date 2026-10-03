;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Writing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable flyspell prog mode when activating prog-mode
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Enable flyspell mode for text mode
(add-hook 'text-mode-hook #'flyspell-mode)

;; Light theme for writing
(use-package almost-mono-themes
             :ensure t)

(defun my/writeroom (arg)
  "Hook used for writeroom-mode."
  (cond
   ((= arg 1)
    (progn
      (setq display-line-numbers nil)
      (visual-line-mode)
      (load-theme 'almost-mono-white t)
      )
    )
   ((= arg -1)
    (progn
      (setq display-line-numbers t)
      (visual-line-mode)
      (load-theme 'catppuccin t)
      )
    )
   )
  )

(use-package visual-fill-column
  :defer t
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :bind (:map minimacs-writing-keymap
         ("s" . writeroom-mode)
         )
  )
