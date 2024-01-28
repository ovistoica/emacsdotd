;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


(defun os/org-path (path)
  (expand-file-name path org-directory))


(defun os/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
          (expand-file-name
           (format-time-string "%Y/%Y-%2m-%B.org")
           (os/org-path "Journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
        (make-directory journal-year-dir))
    journal-file-name))


(defun os/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'os/on-org-capture)

(use-package org
  :straight t
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "READ(r)" "|"  "DONE(d!)")
          (sequence "|" "WAIT(w)" "BACK(b)")))

  (setq org-todo-keyword-faces
        '(("NEXT" . (:foreground "orange red" :weight bold))
          ("WAIT" . (:foreground "HotPink2" :weight bold))
          ("BACK" . (:foreground "MediumPurple3" :weight bold))))

  ;; Configure common tags
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("batch" . ?b)
          ("followup" . ?f)))

  (setq org-agenda-window-setup 'current-window)

  ;; Make done tasks show up in the agenda log
  (setq org-log-done 'time)
  (setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")
  (setq org-agenda-custom-commands
        `(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority")))
            (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-max-todos nil)))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for")
                   (org-agenda-text-search-extra-files nil)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                   (org-agenda-text-search-extra-files nil)))))

          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))))
  (setq org-capture-templates
        `(("t" "Tasks")
          ("tt" "Task" entry (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("ts" "Clocked Entry Subtask" entry (clock)
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("d" "Daily Review" entry
           (file+olp+datetree +org-capture-todo-file "Review")
           "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)

          ("j" "Journal Entries")
          ("je" "General Entry" entry
           (file+olp+datetree ,(os/org-path "Journal.org"))
           "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)
          ("jt" "Task Entry" entry
           (file+olp+datetree ,(os/org-path "Journal.org"))
           "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1)
          ("jj" "Journal" entry
           (file+olp+datetree ,(os/org-path "Journal.org"))
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :tree-type week
           :clock-in :clock-resume
           :empty-lines 1))))
