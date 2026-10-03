;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; A transient menu to make working with the tab-bar easier
;; The `transient' library is built-in and makes defining little menus
;; easy to work with. Activate this menu with `C-c C-t'.
(use-package transient
  :ensure nil                           ; built-in
  :config
  ;; You can define as many of these as you like
  (transient-define-prefix tab-bar-transient ()
    "Tab-bar menu"
    [["Creation"
      ("t" "new tab" tab-bar-new-tab)
      ("n" "next command in new tab" other-tab-prefix)]
     ["Movement"
      ("j" "jump to tab" tab-switch)
      ("h" "move left" tab-bar-move-tab-backward :transient t)
      ("l" "move right" tab-bar-move-tab :transient t)]]
    [["Management"
      ("r" "rename tab" tab-rename)]]
    [[""
      ("RET" "Done" transient-quit-one)]])
  :bind (:map global-map
              ("C-c C-t" . tab-bar-transient)))
