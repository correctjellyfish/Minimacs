;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package initialization
;; Add MELPA to the list of ELPAs that Emacs will read.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Stop the splash Screen
(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes by
;; using the OS file change notification interface rather than
;; repeatedly polling to see if there are changes.
;;
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
;; Set this to `nil' if Emacs is having trouble picking up changes.
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer: future invocations will have
;; recently-used selections sorted first
(savehist-mode)

;; Save existing clipboard content to the kill ring---useful if you've
;; copied something from an external program and then kill some text
;; in Emacs shortly after. Also, deduplicate kill ring contents.
(setopt save-interprogram-paste-before-kill t)
(setopt kill-do-not-save-duplicates t)

;; Don't ping url-looking things when running find-file
(setopt ffap-machine-p-known 'reject)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Rebalance windows automatically when splitting
(setopt window-combination-resize t)

;; On macOS, make the first click raise the window but don't
;; reposition the cursor to where the click happened.
(setopt ns-click-through nil)

;; Prefer horizontal split on landscape monitors: `longest' is
;; default; can be `vertical' or `horizontal'.
;; See also the variable `split-width-threshold'.
(setopt split-window-preferred-direction 'longest)

;; Fix archaic defaults; justification: https://practicaltypography.com/one-space-between-sentences.html
(setopt sentence-end-double-space nil)

;; Make all confirmation prompts use `y' or `n'. Default is for some
;; prompts to ask for a full `yes' or `no' when the operation is
;; potentially dangerous. Commented out to keep the safer behavior.
(setopt use-short-answers t)

;; Make right-click do something sensible and shift-drag behave better
(when (display-graphic-p)
  (mouse-shift-adjust-mode)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun minimacs--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'minimacs--backup-file-name)

;; Remove autosave buffers when killing buffer
(setq kill-buffer-delete-auto-save-files t)

;; Basic speedups
;; assume left-to-right text in all buffers.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Replace selection when typing (or pasting)
(delete-selection-mode 1)

