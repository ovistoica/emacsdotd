;;; Code:
(setq inhibit-startup-message t)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable the toolbar
(tooltip-mode -1)          ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room

(menu-bar-mode -1)         ; Disable the menu bar

                                        ; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



