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

;; Citation help
(use-package citar
             :ensure t
             :hook (typst-ts-mode . citar-capf-setup)
             :bind (:map minimacs-writing-keymap
                    ("c" . citar-insert-citation))
             )
(use-package citar-typst
             :ensure t
             :commands (citar-typst-mode)
             :hook (typst-ts-mode . citar-typst-mode)
             )

;; Focus by dimming text outside of region
(use-package focus 
             :commands (focus-mode)
             :config 
             (add-to-list 'focus-mode-to-thing '(typst-ts-mode . paragraph))
             (add-to-list 'focus-mode-to-thing '(markdown-mode . paragraph))
             (add-to-list 'focus-mode-to-thing '(text-mode . paragraph))
             )

(defun my/writeroom (arg)
  "Hook used for writeroom-mode, ARG indicates if entering or leaving."
  (cond
   ((= arg 1)
    (progn
      (setq display-line-numbers nil)
      (visual-line-mode 1)
      (enable-theme minimacs-writing-theme)
      (eldoc-box-hover-mode -1)
      (eldoc-box-hover-at-point-mode -1)
      (focus-mode 1)
      )
    )
   ((= arg -1)
    (progn
      (setq display-line-numbers 'relative)
      (visual-line-mode -1)
      (disable-theme minimacs-writing-theme)
      (eldoc-box-hover-mode 1)
      (eldoc-box-hover-at-point-mode 1)
      (focus-mode -1)
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
