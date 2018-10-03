;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Monospace" :size 18))

(after! mu4e

  ;; setting up email in mu4e
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (set-email-account! "RUphysics"
      '((mu4e-sent-folder       . "/RUphysics/Sent")
        (mu4e-drafts-folder     . "/RUphysics/Drafts")
        (mu4e-trash-folder      . "/RUphysics/Trash")
        (mu4e-refile-folder     . "/RUphysics/Archive")
        (smtpmail-smtp-user     . "jrb285@physics.rutgers.edu")
        (user-mail-address      . "jrb285@physics.rutgers.edu")
        (user-full-name         . "John Bonini"))
      t)
  ;;; Set up some common mu4e variables
  (setq mu4e-update-interval 300
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-html2text-command "w3m -T text/html"
        mu4e-view-show-addresses t)
  ;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/RUphysics/INBOX" . ?j)))
  
  ;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed AND NOT maildir:/RUphysics/Archive" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("flag:flagged" "flagged" ?f)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i))))

(after! org
  (setq org-image-actual-width 700)
  ;; make code look nice even before session started
  (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; I like when org opens links in new windows/frames
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)

  (if (featurep! :private frames-only)
    (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
    (setq org-src-window-setup 'other-window)
    )

  (add-to-list 'org-file-apps '("\\.xoj\\'" . "xournal %s"))
  (setq org-ref-default-bibliography '("~/org/references/misc/references.bib")
        org-ref-pdf-directory "~/Documents/papers/misc/"
        org-ref-bibliography-notes "~/org/references/misc/notes.org")
 (setq org-refile-targets (quote (("master.org" :maxlevel . 1)
                                  ("archive.org" :maxlevel . 1)
                                  (org-agenda-files :maxlevel . 1))))

 (defun esf/execute-startup-block ()
   (interactive)
   (org-babel-goto-named-src-block "startup")
   (org-babel-execute-src-block)
   (beginning-of-buffer)
   (org-overview))

 (setq org-tag-persistent-alist '((:startgroup . nil)
                                  ("@work" . ?w) ("@personal" . ?h)
                                  (:endgroup . nil)
                                  ("reading" . ?r)
                                  ("coding" . ?c)
                                  ("investigating" . ?i)
                                  ("organizing" . ?o)
                                  ("writing/preparing" . ?p)
                                  ("calculations" . ?s)))
 (setq org-todo-keywords
       '((sequence "TODO" "IN-PROGRESS" "WAITING" "APPT" "CANCELED" "DEFERRED" "DONE")))
 (setq org-capture-templates
       '(("t" "Todo" entry (file+headline "~/org/master.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("s" "Someday" entry (file+headline "~/org/someday.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("a" "Appointments" entry (file+headline "~/org/master.org" "Misc appointments")
          "* APPT %?\n  %i %a %U")
         ))


  (after! ob-ipython
    (setq ob-ipython-resources-dir ".ob-ipython-resrc/")
    (defun +org*org-babel-edit-prep:ipython-complete (info)
      (let* ((params (nth 2 info))
             (session (cdr (assoc :session params))))
        (org-babel-ipython-initiate-session session params))
      ;; Support for python.el's "send-code" commands within edit buffers.
      (setq-local python-shell-buffer-name
                  (format "Python:ob-ipython-%s"
                          (ob-ipython--normalize-session
                           (cdr (assoc :session (nth 2 info))))))
      (setq-local default-directory
                  (format "%s"
                          (ob-ipython--normalize-session
                           (cdr (assoc :pydir (nth 2 info))))))
      (ob-ipython-mode 1)
      ;; A few things to to do completions properly
      ;; 1.  use company-ob-ipython then company-anaconda
      ;;     anaconda will work before a block is executed
      ;;     and is useful for getting documentation
      ;; 2. set company-idle-delay to nil and bind a key for completion (C-n)
      ;;    this is because company-ob-ipython is slow
      ;;    see https://github.com/gregsexton/ob-ipython/issues/151
      ;;    Two possible ways:
      ;;      a. (current) bind to company-ob-ipython (then don't put it in backends and keep delay finite)
      ;;      b. bind to company-indent-or-complete-common
      ;;    ISSUE: the complete binding seems to not work with :local
      ;; 3. for docs from parts anaconda can't track we bind a key to
      ;;    ob-ipython-complete (C-S-k)
      ;;    could replace with conditional map (when anaconda fails) so K can be used
      (when (featurep! :completion company)
        (setq-local company-backends
                    '(company-anaconda
                      company-dabbrev
                      company-files
                      company-yasnippet))
        (setq-local company-idle-delay 0.1))
      (map! :local
            :desc "ob-ipython-inspect" :n "C-S-k" #'ob-ipython-inspect
            :desc "ob ipython completion" :i "C-n" #'company-ob-ipython)
      (when (featurep 'lpy)
        (setq lispy-python-proc
              (format "Python:ob-ipython-%s"
                      (ob-ipython--normalize-session
                       (cdr (assoc :session (nth 2 info)))))
              lispy--python-middleware-loaded-p nil)
        (lispy--python-middleware-load))
      )
      ;; 4. fix repl completion? company-capf seems to work inconsistently (C-x C-o)
      ;;    python-shell-completion-complete-or-indent seems to work (TAB = C-i)
      ;;    but is annoying when there are multiple choices
    (advice-add '+org*org-babel-edit-prep:ipython :override #'+org*org-babel-edit-prep:ipython-complete)
    )
  )

(after! python
  (setenv "PYTHONPATH" "/home/john/scripts/pyMods/:"))

;; key binds
(map! :leader
      (:desc "App" :prefix "a"
        :desc "Ielm" :n "i" #'ielm
        :desc "Mail" :n "m" #'=email
        :desc "Processes" :n "p" #'list-processes))

(map! :leader
      :mode process-menu-mode
      :desc "Kill process" :n "k" #'process-menu-delete-process)

(defun copy-window ()
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos))
  )

(map!
 :desc "open copy of current window" :m "go" 'copy-window)
