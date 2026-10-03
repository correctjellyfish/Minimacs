;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Casual
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Casual (for dired transient mode)
(use-package casual
  :ensure t
  :after dired
  :bind (
	 :map dired-mode-map
	 ("C-o" . #'casual-dired-tmenu)
	 ("s" . #'casual-dired-sort-by-tmenu)
	 ("/" . #'casual-dired-search-replace-tmenu)
	 ( "M-o" . #'dired-omit-mode)
	 ( "E" . #'wdired-change-to-wdired-mode)
	 ( "M-n" . #'dired-next-dirline)
	 ("M-p" . #'dired-prev-dirline)
	 ("]" . #'dired-next-subdir)
	 ("[" . #'dired-prev-subdir)
	 ("A-M-<mouse-1>" . #'browse-url-of-dired-file)
	 ("<backtab>" . #'dired-prev-subdir)
	 ("TAB" . #'dired-next-subdir)
	 ("M-j" . #'dired-goto-subdir)
	 (";" . #'image-dired-dired-toggle-marked-thumbs)
	 :map image-dired-thumbnail-mode-map
	 ("n" . #'image-dired-display-next)
	 ("p" . #'image-dired-display-previous)
	 )
  :hook (
	 (dired-mode . hl-line-mode)
	 )
  )
