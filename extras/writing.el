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
  :ensure t
  :config
  (load-theme 'almost-mono-white t t))

(defun my/writeroom (arg)
  "Hook used for writeroom-mode, ARG indicates if entering or leaving."
  (cond
   ((= arg 1)
    (progn
      (setq display-line-numbers nil)
      (visual-line-mode)
      (enable-theme 'almost-mono-white)
      (eldoc-box-hover-mode -1)
      (eldoc-box-hover-at-point-mode -1)
      )
    )
   ((= arg -1)
    (progn
      (setq display-line-numbers 'relative)
      (visual-line-mode)
      (disable-theme 'almost-mono-white)
      (eldoc-box-hover-mode 1)
      (eldoc-box-hover-at-point-mode 1)
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
              ("z" . writeroom-mode))
  )
