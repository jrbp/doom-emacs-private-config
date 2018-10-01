;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(set-face-attribute 'default nil :height 160)

(after! mu4e
  ;;; setting up sending mail with msmtp
  ;(setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;(setq sendmail-program "msmtp")
  ;;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
  ;(setq message-sendmail-extra-arguments '("--read-envelope-from"))
  ;(setq message-sendmail-f-is-evil 't)
  
  ;; setting up email in mu4e
  ;;
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  ;(setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (set-email-account! "RUphysics"
      '((mu4e-sent-folder       . "/RUphysics/Sent")
        (mu4e-drafts-folder     . "/RUphysics/Drafts")
        (mu4e-trash-folder      . "/RUphysics/Trash")
        (mu4e-refile-folder     . "/RUphysics/Archive")
        (smtpmail-smtp-user     . "jrb285@physics.rutgers.edu")
        (user-mail-address      . "jrb285@physics.rutgers.edu")
        (user-full-name         . "John Bonini"))
      t)
  ;;(setq mu4e-account-alist
  ;;      '(("RUphysics"
  ;;         ;; Under each account, set the account-specific variables you want.
  ;;         (mu4e-sent-messages-behavior sent)
  ;;         (mu4e-sent-folder "/RUphysics/Sent")
  ;;         (mu4e-drafts-folder "/RUphysics/Drafts")
  ;;         (mu4e-refile-folder "/RUphysics/Archive")
  ;;         (mu4e-trash-folder "/RUphysics/Trash")
  ;;         (user-mail-address "jrb285@physics.rutgers.edu")
  ;;         (user-full-name "John Bonini"))
  ;;        ))
  ;;(mu4e/mail-account-reset)
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

 (setq org-agenda-files
   '("~/Dropbox/org/transparent_conductors.org"
     "~/Dropbox/org/pto_divacancies.org"
     "~/Dropbox/org/perovskite_database.org"
     "~/Dropbox/org/wannier_polarization.org"
     "~/Dropbox/org/master.org"
     "~/Dropbox/org/dielectric_slab_model.org"))

  (setq org-image-actual-width 700)
  ;; make code look nice even before session started
  (add-to-list 'org-src-lang-modes '("ipython" . python))
  ;; I like when org opens links in new windows/frames
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)
  (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit

  (add-to-list 'org-file-apps '("\\.xoj\\'" . "xournal %s"))
  (setq org-ref-default-bibliography '("~/Dropbox/org/references/misc/references.bib")
        org-ref-pdf-directory "~/Documents/papers/misc/"
        org-ref-bibliography-notes "~/Dropbox/org/references/misc/notes.org")
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
       '(("t" "Todo" entry (file+headline "~/Dropbox/org/master.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("s" "Someday" entry (file+headline "~/Dropbox/org/someday.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("a" "Appointments" entry (file+headline "~/Dropbox/org/master.org" "Misc appointments")
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
      ;; hack on company mode to use company-capf rather than company-anaconda
      (when (featurep! :completion company)
        (setq-local company-backends
                    '(company-ob-ipython
                      company-anaconda
                      company-dabbrev
                      company-files
                      company-yasnippet))
        (setq-local company-idle-delay 0.1))
      (when (featurep 'lpy)
        (setq lispy-python-proc
              (format "Python:ob-ipython-%s"
                      (ob-ipython--normalize-session
                       (cdr (assoc :session (nth 2 info)))))
              lispy--python-middleware-loaded-p nil)
        (lispy--python-middleware-load)))
    ;TODO: in the above can we bind something to ob-ipython-inspect
    ;      (would be K, but the anaconda bits are actually useful when we havn't executed yet)
    ;TODO: repl only has completion with C-x C-o
    (advice-add '+org*org-babel-edit-prep:ipython :override #'+org*org-babel-edit-prep:ipython-complete)
    )
  )

(after! python
  (setenv "PYTHONPATH" "/home/john/scripts/pyMods/:"))
