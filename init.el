;;; -*- lexical-binding: t -*-
;;__/\\\\____________/\\\\_________________________________________________________________________________________
;; _\/\\\\\\________/\\\\\\_________________________________________________________________________________________
;;  _\/\\\//\\\____/\\\//\\\__/\\\________________/\\\_______________________________________________________________
;;   _\/\\\\///\\\/\\\/_\/\\\_\///___/\\/\\\\\\___\///_____/\\\\\__/\\\\\____/\\\\\\\\\________/\\\\\\\\__/\\\\\\\\\\_
;;    _\/\\\__\///\\\/___\/\\\__/\\\_\/\\\////\\\___/\\\__/\\\///\\\\\///\\\_\////////\\\_____/\\\//////__\/\\\//////__
;;     _\/\\\____\///_____\/\\\_\/\\\_\/\\\__\//\\\_\/\\\_\/\\\_\//\\\__\/\\\___/\\\\\\\\\\___/\\\_________\/\\\\\\\\\\_
;;      _\/\\\_____________\/\\\_\/\\\_\/\\\___\/\\\_\/\\\_\/\\\__\/\\\__\/\\\__/\\\/////\\\__\//\\\________\////////\\\_
;;       _\/\\\_____________\/\\\_\/\\\_\/\\\___\/\\\_\/\\\_\/\\\__\/\\\__\/\\\_\//\\\\\\\\/\\__\///\\\\\\\\__/\\\\\\\\\\_
;;        _\///______________\///__\///__\///____\///__\///__\///___\///___\///___\////////\//_____\////////__\//////////__


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/basic.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Setup Keymaps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;; Buffer ;;;
;;;;;;;;;;;;;;
(defvar-keymap minimacs-buffer-keymap
  :doc "Buffer related keys"
  :prefix t
  "k" #'kill-current-buffer
  "i" #'ibuffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "r" #'revert-buffer
  )
(global-set-key (kbd "C-c b") 'minimacs-buffer-keymap)

;;;;;;;;;;;;;;;
;;; Comment ;;;
;;;;;;;;;;;;;;;
(defvar-keymap minimacs-comment-keymap
  :doc "Comment related keys"
  :prefix t
  "l" #'comment-line
  "r" #'comment-or-uncomment-region
  "d" #'comment-dwim
  )
(global-set-key (kbd "C-c c") 'minimacs-comment-keymap)

;;;;;;;;;;;;;;
;;; Errors ;;;
;;;;;;;;;;;;;;
(defvar-keymap minimacs-errors-keymap
  :doc "Errors/Flycheck keys"
  :prefix t
  )
(global-set-key (kbd "C-c e") 'minimacs-errors-keymap)

;;;;;;;;;;;;;
;;; Files ;;;
;;;;;;;;;;;;;
(defvar-keymap minimacs-file-keymap
  :doc "File related keys"
  :prefix t
  "d" #'dired
  "w" #'write-file
  "s" #'save-buffer
  )
(global-set-key (kbd "C-c f") 'minimacs-file-keymap)

;;;;;;;;;;;
;;; VCS ;;;
;;;;;;;;;;;
(defvar-keymap minimacs-vcs-keymap
  :doc "VCS related keys"
  :prefix t
  )
(global-set-key (kbd "C-c v") 'minimacs-vcs-keymap)

;;;;;;;;;;;;
;;; Jump ;;;
;;;;;;;;;;;;
(defvar-keymap minimacs-jump-keymap
  :doc "Movement related keys"
  :prefix t
  )
(global-set-key (kbd "C-c j") 'minimacs-jump-keymap)

;;;;;;;;;;;;;;;;
;;; Language ;;;
;;;;;;;;;;;;;;;;
(defvar-keymap minimacs-language-keymap
  :doc "Language/LSP related keys"
  :prefix t
  )
(global-set-key (kbd "C-c l") 'minimacs-language-keymap)

;;;;;;;;;;;;;;
;;; Search ;;;
;;;;;;;;;;;;;;
(defvar-keymap minimacs-search-keymap
  :doc "Search related keys"
  :prefix t
  )
(global-set-key (kbd "C-c s") 'minimacs-search-keymap)


;;;;;;;;;;;;;;;;
;;; Terminal ;;;
;;;;;;;;;;;;;;;;
(defvar-keymap minimacs-term-keymap
  :doc "Terminal keys"
  :prefix t
  )
(global-set-key (kbd "C-c t") 'minimacs-term-keymap)

;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;
(defvar-keymap minimacs-utils-keymap
  :doc "Misc. Utility keys"
  :prefix t
  )
(global-set-key (kbd "C-c u") 'minimacs-utils-keymap)

;;;;;;;;;;;;;;
;;; Window ;;;
;;;;;;;;;;;;;;
(defvar-keymap minimacs-windows-keymap
  :doc "Window related keys"
  :prefix t
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "q" #'delete-window
  "v" #'split-window-right
  "s" #'split-window-right
  )
(global-set-key (kbd "C-c w") 'minimacs-windows-keymap)

;;;;;;;;;;;;
;;; REPL ;;;
;;;;;;;;;;;;
(defvar-keymap minimacs-repl-keymap
  :doc "REPL keys"
  :prefix t
  )
(global-set-key (kbd "C-c r") 'minimacs-repl-keymap)

;;;;;;;;;;;;;;;
;;; Writing ;;;
;;;;;;;;;;;;;;;
(defvar-keymap minimacs-writing-keymap
  :doc "Writing keys"
  :prefix t
  )
(global-set-key (kbd "C-c z") 'minimacs-writing-keymap)



;; which-key setup
(use-package which-key
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "extras/minibuffer.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   User Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/user_interface.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Casual
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/casual.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "extras/completion.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Templates
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "extras/templates.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "extras/movement.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Search
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/search.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Context Menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/context_menu.el" user-emacs-directory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Terminal/Eshell
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/terminal.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Writing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/writing.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   VCS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/vcs.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language Modes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/language_modes.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/lsp.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Editing Enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/editing.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Debug
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/debug.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Modal Editing (Meow)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "extras/meow.el" user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c4df9006b9eb32599d758800a32f3487c2cdf13826084511783b47d419024af2"
     default))
 '(package-selected-packages '(citar-typst which-key))
 '(warning-suppress-log-types
   '((files missing-lexbind-cookie
            "/usr/share/emacs/site-lisp/suse-start-po-mode.el")
     (files missing-lexbind-cookie
            "/usr/share/emacs/site-lisp/site-start.d/vterm-init.el")
     (comp) (bytecomp)))
 '(writeroom-global-effects
   '(writeroom-set-fullscreen writeroom-set-alpha
                              writeroom-set-menu-bar-lines
                              writeroom-set-tool-bar-lines
                              writeroom-set-vertical-scroll-bars
                              writeroom-set-bottom-divider-width
                              my/writeroom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))
