;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq dw/is-termux nil)


(setq dw/is-guix-system t)



;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Clean up unused repos with `straight-remove-unused-repos'


;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)


;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)



;; Add my library path to load-path
(add-to-list 'load-path "~/.dotfiles/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

(set-default-coding-systems 'utf-8)

(server-start)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; set command and mac key to meta
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;;; Navigation keys similar to IntelliJ
(global-set-key (kbd "s-w") 'kill-buffer)
(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package undo-tree
  :diminish t
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'lispy evil-collection-mode-list)
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))



(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))



(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))



;; Thanks, but no thanks
(setq inhibit-startup-message t)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)


(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX




(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



(setq large-file-warning-threshold nil)



(setq vc-follow-symlinks t)


(setq ad-redefinition-action 'accept)



;; THEME


(use-package spacegray-theme :defer t)
(use-package doom-themes :defer t)
(unless dw/is-termux
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))


;; FONT
;; TODO fix fonts


;; Set the font face based on platform
;; (pcase system-type
;;   ((or 'gnu/linux 'windows-nt 'cygwin)
;;    (set-face-attribute 'default nil
;;                        :font "JetBrains Mono"
;;                        :weight 'light
;;                        :height (dw/system-settings-get 'emacs/default-face-size)))
;;   ('darwin (set-face-attribute 'default nil :font "JetBrains Mono" :height 170)))

;; ;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil
;;                     :font "JetBrains Mono"
;;                     :weight 'light
;;                     :height (dw/system-settings-get 'emacs/fixed-face-size))

;; ;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil
;;                     ;; :font "Cantarell"
;;                     :font "Iosevka Aile"
;;                     :height (dw/system-settings-get 'emacs/variable-face-size)
;;                     :weight 'light)


;; PROPER UNICODE glyph support 


(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                     (lambda (i) (string-equal (car i) block-name))
                     unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :disabled
  :if (not dw/is-termux)
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))


;; EMOJ


(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)


;; MODELINE


(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

;; Diminish hides pesky minor modes from the modelines
(use-package diminish)

;; Doom modeline 

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))


(dw/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))


(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))


(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/Athens" "Athens")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")



(defun dw/show-server-edit-buffer (buffer)
  ;; TODO: Set a transient keymap to close with 'C-c C-c'
  (split-window-vertically -15)
  (other-window 1)
  (set-buffer buffer))

;; (setq server-window #'dw/show-server-edit-buffer)



(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)


(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))



;; TODO Change to better suite me
(dw/leader-key-def
  "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
  "fd"  '(:ignore t :which-key "dotfiles")
  "fdd" '((lambda () (interactive) (find-file "~/.dotfiles/Desktop.org")) :which-key "desktop")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Emacs.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/Emacs.org")) :which-key "edit config")
  "fdm" '((lambda () (interactive) (find-file "~/.dotfiles/Mail.org")) :which-key "mail")
  "fdM" '((lambda () (interactive) (counsel-find-file "~/.dotfiles/.config/guix/manifests/")) :which-key "manifests")
  "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
  "fdp" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
  "fdw" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Workflow.org"))) :which-key "workflow")
  "fdv" '((lambda () (interactive) (find-file "~/.dotfiles/.config/vimb/config")) :which-key "vimb"))



(use-package hydra
  :defer 1)


;; COMPLETIONS 


(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))



(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

(use-package vertico
  ;; :straight '(vertico :host github
  ;;                     :repo "minad/vertico"
  ;;                     :branch "main")
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))




(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))



(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))



(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))


(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 30)

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("s-f" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))



(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

;; Thanks Karthik!
(with-eval-after-load 'eshell-mode
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))



(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))



(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :after embark)

;; (use-package embark-consult
;;   :straight '(embark-consult :host github
;;                              :repo "oantolin/embark"
;;                              :files ("embark-consult.el"))
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))


(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))


;; (defun dw/center-buffer-with-margins ()
;;   (let ((margin-size (/ (- (frame-width) 80) 3)))
;;     (set-window-margins nil margin-size margin-size)))


(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))


;; CONTROL BUFFER PLACEMENT

;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


;; Expand region
;; TODO add more to expand region
(use-package expand-region
  :if (not dw/is-termux)
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))



;; TODO Fix dired
;; (use-package dired
;;   :ensure nil
;;   :straight nil
;;   :defer 1
;;   :commands (dired dired-jump)
;;   :config
;;   (setq dired-listing-switches "-agho --group-directories-first"
;;         dired-omit-files "^\\.[^.].*"
;;         dired-omit-verbose nil
;;         dired-hide-details-hide-symlink-targets nil
;;         delete-by-moving-to-trash t))

;; ORG



(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-magit #'function-display-buffer-same-window-except-diff-v1))

(dw/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)



(use-package magit-todos
  :defer t)


(use-package git-gutter
  :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (unless dw/is-termux
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"))

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))



(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace")))
  (setq projectile-switch-project-action #'dw/switch-project-action))

(use-package counsel-projectile
  :disabled
  :after projectile
  :config
  (counsel-projectile-mode))

(dw/leader-key-def
  "pf"  'projectile-find-file
  "ps"  'projectile-switch-project
  "pF"  'consult-ripgrep
  "pp"  'projectile-find-file
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)


;;; LSP

(use-package eglot
  :straight nil
  :ensure nil
  :hook ((typescript-mode . eglot-ensure)))


;; Clojure

(use-package cider
  :mode "\\.clj[sc]?\\'"
  :config
  (evil-collection-cider-setup))


;; javascript typescript

;;; duomacs-javascript-typescript.el --- duomacs setup for Javascript & Typescript -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; Code:

(require 'eglot)
(require 'flymake)
(require 'treesit)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package
  flymake-eslint
  :straight t)

(defun os/tsx-mode-hook ()
  "Internal function.  Hook to be run upon entering `tsx-mode'."
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     (setq-local
      flymake-eslint-project-root
      (let* ((package-json
              (locate-dominating-file (buffer-file-name (current-buffer)) "package.json")))
        (when package-json
          (file-name-directory (expand-file-name package-json))))
      flymake-eslint-executable-name
      "eslint_d"
      eldoc-documentation-strategy
      'eldoc-documentation-compose)
     (flymake-eslint-enable))
   nil t))

;; (put 'eglot-node 'flymake-overlay-control nil)
;; (put 'eglot-warning 'flymake-overlay-control nil)
;; (put 'eglot-error 'flymake-overlay-control nil)))

;; format on save
(use-package
  apheleia
  :delight
  :straight t
  :config
  (add-to-list
   'apheleia-mode-alist
   '(tsx-ts-mode . prettier-typescript)))

;; code-coverage overlays
(use-package
  coverlay
  :delight coverlay-minor-mode
  :straight t)

;; CSS-in-JS support for tsx-mode
(use-package
  css-in-js-mode
  :delight
  :straight
  '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js" :branch "main" :post-build ((require 'css-in-js-mode) (css-in-js-mode-fetch-shared-library))))

;; linter adapter which doesn't use LSP
(use-package
  flymake-eslint
  :straight t)

;; code-folding
;; origami depends on some now-deprecated cl functions and there's not much we
;; can do about that
(let ((byte-compile-warnings '((not cl-functions))))
  (use-package
    origami
    :delight
    :straight t))

;; major-mode for JS/TS/JSX/TSX
(use-package
  tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs29")
  :mode (("\\.[jt]s[x]?\\'" . tsx-mode)
         ("\\.[mc]js\\'" . tsx-mode))
  :config
  (add-hook
   'tsx-mode-hook
   #'duomacs/tsx-mode-hook))

;; LSP inlay hints
(add-to-list
 'eglot-server-programs
 '((tsx-mode)
   "typescript-language-server" "--stdio"
   :initializationOptions
   (:preferences
    (:includeInlayParameterNameHints
     "all"
     :includeInlayParameterNameHintsWhenArgumentMatchesName
     t
     :includeInlayFunctionParameterTypeHints
     t
     :includeInlayVariableTypeHintsWhenTypeMatchesName
     t
     :includeInlayPropertyDeclarationTypeHints
     t
     :includeInlayFunctionLikeReturnTypeHints
     t
     :includeInlayEnumMemberValueHints
     nil))))


;;(provide 'duomacs-javascript-typescript)
;;; duomacs-javascript-typescript.el ends here



(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (progn
    (define-key sp-keymap (kbd "s-{") 'sp-wrap-curly)
    (define-key sp-keymap (kbd "s-[") 'sp-wrap-square)
    (define-key sp-keymap (kbd "s-(") 'sp-wrap-round)
    (define-key sp-keymap (kbd "s-J") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "s-K") 'sp-forward-slurp-sexp)

    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-S-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-S-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    (define-key sp-keymap (kbd "C-M-S-<backspace>") 'sp-splice-sexp-killing-forward)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))

;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         tsx-mode))



(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))



(use-package vterm
  :after evil-collection
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))
