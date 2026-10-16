;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Context Menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :after (avy embark-consult)
  :bind (
         ("C-c a" . embark-act)   ; bind this to an easy key to hit
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Add the option to run embark when using avy
  (defun minimacs/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'minimacr/avy-action-embark)
  )

(use-package embark-consult
   :ensure t)
