;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Monospace" :size 18))

(setq initial-buffer-choice "~/org/master.org")

;; TODO: it would be a good idea to start all my custom functions with jrb/ or something
(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], insert the file name exactly as
  it appears in the minibuffer prompt.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to expand the file name to
  its fully canocalized path.  See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (expand-file-name filename)))))


; (defun open-termite ()
;   (interactive)
;   (call-process-shell-command "termite&" nil 0))
(defun open-iterm ()
  ;; currently useless as it always opens in home
  (interactive)
  (call-process-shell-command "open -aiterm&" nil 0))
;
; (defun open-ranger ()
;   (interactive)
;   (call-process-shell-command "termite -e ranger&" nil 0))

; terrible hack for sshfs files to not be super slow
; (defun open-as-sym-link (f)
;   (interactive "fFile:")
;   (let ((sl
;         (concat
;          (file-name-as-directory "/home/john/.tmp_sshfs_symlinks")
;          (file-name-nondirectory f))))
;     (if (not (file-exists-p sl))
;         (f-symlink f sl))
;     (switch-to-buffer (find-file sl))))

; (after! mu4e
;   (add-to-list 'mu4e-view-actions
;                '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;  ; doom doesn't load org-mu4e until composing
;  ; this stops us from using its org-capture function
;   (require 'org-mu4e nil 'noerror)
;   ;; setting up email in mu4e
;   (setq message-send-mail-function 'message-send-mail-with-sendmail)
;   (setq sendmail-program "msmtp")
;   (set-email-account! "RUphysics"
;       '((mu4e-sent-folder       . "/RUphysics/Sent")
;         (mu4e-drafts-folder     . "/RUphysics/Drafts")
;         (mu4e-trash-folder      . "/RUphysics/Trash")
;         (mu4e-refile-folder     . "/RUphysics/Archive")
;         (smtpmail-smtp-user     . "jrb285@physics.rutgers.edu")
;         (user-mail-address      . "jrb285@physics.rutgers.edu")
;         (user-full-name         . "John Bonini"))
;       t)
;   ;;; Set up some common mu4e variables
;   (setq mu4e-update-interval 300
;         mu4e-compose-signature-auto-include nil
;         mu4e-view-show-images t
;         mu4e-html2text-command "w3m -T text/html"
;         mu4e-view-show-addresses t)
;   ;; Mail directory shortcuts
;   (setq mu4e-maildir-shortcuts
;         '(("/INBOX" . ?j)))
;
;   ;;; Bookmarks
;   (setq mu4e-bookmarks
;         `(("flag:unread AND NOT flag:trashed AND NOT maildir:/RUphysics/Archive" "Unread messages" ?u)
;           ("date:today..now" "Today's messages" ?t)
;           ("date:7d..now" "Last 7 days" ?w)
;           ("flag:flagged" "flagged" ?f)
;           ("mime:image/*" "Messages with images" ?p)
;           (,(mapconcat 'identity
;                        (mapcar
;                         (lambda (maildir)
;                           (concat "maildir:" (car maildir)))
;                         mu4e-maildir-shortcuts) " OR ")
;            "All inboxes" ?i))))

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))


; never did the google developers steps
; (defun my-open-calendar ()
;   (interactive)
;   (cfw:open-calendar-buffer
;    :contents-sources
;    (list
;     (cfw:org-create-source "Green")  ; orgmode source
;     ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
;    )))
;
; (defun cfw:open-org-calendar-with-cal1 ()
;   (interactive)
;   (let ((org-agenda-files '("/Users/jbonini/Dropbox_simons/org/gcal.org"))) ;;can use directory
;     (call-interactively #'+calendar/open-calendar)))
;
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! org
  (after! org-noter
    (setq org-noter-default-notes-file-names '("notes.org"))
    (setq org-noter-notes-search-path '("/Users/jbonini/Dropbox_simons/org/references/misc" "/Users/jbonini/Dropbox_simons/org/"))
    (map! :mode pdf-view-mode
          :desc "insert a note"
          :n "i" ))

  (set-company-backend! 'org-mode
    'company-capf) ; put this in front so completions work for jupyter
  ;; macro to convert old ob-ipython blocks to emacs-jupyter blocks
  (fset 'obipy-to-jup
        (lambda (&optional arg) "Keyboard macro." (interactive "p")
          (kmacro-exec-ring-item (quote ([3 22 117 69 108 108 67 106 117 112 121 116 101 114 45 112 121 116 104 111 110 32 58 115 101 115 115 105 111 110 32 112 121 32 58 97 115 121 110 99 32 121 101 115 escape] 0 "%d")) arg)))
  ;; so that in inspect buffer we can sort of go to the definition (at least the file)
  (map! :mode help-mode
        :desc "find-file-at-point"
        :n [C-return] 'find-file-at-point)


  ;; for inline latex
  (plist-put org-format-latex-options :scale 3)
  (setq org-latex-packages-alist '(("" "braket" t)))

  ; (add-hook! 'org-mode-hook (setq-local display-line-numbers 'nil)) ; for newer version of doom i reverted from
  ; org was getting slow, disabling some things for speed up here:
  (remove-hook! 'org-mode-hook #'+org|enable-auto-update-cookies)
  (advice-remove #'evil-org-open-below #'+org*evil-org-open-below) ; didn't like this anyway
  ; realzied there were some bad json dumps in some files, cleaning up the really long lines helped a lot
  ; see goto-long-line which has been added to this config
  ;
  ; I didn't like the auto completeion of the formatting stuff
  ; below caused issue on osx, commenting it out and still have the desired behavior
  ; maybe new version of doom got rid of smartparen for these
  ; (sp-with-modes 'org-mode
  ;   (sp-local-pair "*" nil :actions :rem)
  ;   (sp-local-pair "_" nil :actions :rem)
  ;   (sp-local-pair "/" nil :actions :rem)
  ;   (sp-local-pair "~" nil :actions :rem)
  ;   (sp-local-pair "=" nil :actions :rem))


  ; Didn't like that new headings on C-return weren't put in at point
  (setq org-insert-heading-respect-content nil)
  (map! :map evil-org-mode-map
        :desc "New header at point"
        :ni [C-return] 'org-insert-heading)

  (add-to-list 'org-file-apps '("\\.vesta\\'" . "VESTA %s"))
  (add-to-list 'org-file-apps '("\\.nb\\'" . "mathematica %s"))
  ; (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps '("\\.pptx\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.odp\\'" . "libreoffice %s"))
  (setq org-export-with-sub-superscripts (quote {}))
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
 (setq org-refile-targets (quote (("master.org" :maxlevel . 1)
                                  ("archive.org" :maxlevel . 1)
                                  (org-agenda-files :maxlevel . 1))))
 ; default agenda view is just today
 (setq org-agenda-span 'day)
 (setq org-agenda-start-day nil)

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
       '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "CANCELED" "DEFERRED" "DONE")))
 (setq org-capture-templates
       '(("t" "TODO" entry (file+headline "~/org/master.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("l" "Log (misc)" entry (file+headline "~/org/misc-log.org" "Tasks")
          "* %?\n  %i %a %U")
         ("s" "Someday" entry (file+headline "~/org/someday.org" "Tasks")
          "* TODO %?\n  %i %a %U")
         ("a" "Appointments" entry (file+headline "~/org/master.org" "Appointments")
          "* %?\n  %i %a %U")
         ("n" "Notes" entry (file+headline "~/org/master.org" "Notes")
          "* %?\n  %i %a %U")
         ))


  ;; (after! ob-ipython
  ;;   ;; made ob-ipython work for newer version of doom I reverted from
  ;;   ;; (after! ob-async
  ;;   ;;   (add-to-list 'ob-async-no-async-languages-alist "ipython"))

  ;;   (setq ob-ipython-command "/home/john/.pyenv/shims/jupyter")
  ;;   (setq ob-ipython-resources-dir ".ob-ipython-resrc/")
  ;;   (defun +org*org-babel-edit-prep:ipython-complete (info)
  ;;     (let* ((params (nth 2 info))
  ;;            (session (cdr (assoc :session params))))
  ;;       (org-babel-ipython-initiate-session session params))
  ;;     ;; Support for python.el's "send-code" commands within edit buffers.
  ;;     (setq-local python-shell-buffer-name
  ;;                 (format "Python:ob-ipython-%s"
  ;;                         (ob-ipython--normalize-session
  ;;                          (cdr (assoc :session (nth 2 info))))))
  ;;     (setq-local default-directory
  ;;                 (format "%s"
  ;;                         (ob-ipython--normalize-session
  ;;                          (cdr (assoc :pydir (nth 2 info))))))
  ;;     (ob-ipython-mode 1)
  ;;     ;; A few things to to do completions properly
  ;;     ;; 1.  use company-ob-ipython then company-anaconda
  ;;     ;;     anaconda will work before a block is executed
  ;;     ;;     and is useful for getting documentation
  ;;     ;; 2. set company-idle-delay to nil and bind a key for completion (C-n)
  ;;     ;;    this is because company-ob-ipython is slow
  ;;     ;;    see https://github.com/gregsexton/ob-ipython/issues/151
  ;;     ;;    Two possible ways:
  ;;     ;;      a. (current) bind to company-ob-ipython (then don't put it in backends and keep delay finite)
  ;;     ;;      b. bind to company-indent-or-complete-common
  ;;     ;;    ISSUE: the complete binding seems to not work with :local
  ;;     ;; 3. for docs from parts anaconda can't track we bind a key to
  ;;     ;;    ob-ipython-complete (C-S-k)
  ;;     ;;    could replace with conditional map (when anaconda fails) so K can be used
  ;;     (when (featurep! :completion company)
  ;;       (setq-local company-backends
  ;;                   '(company-anaconda
  ;;                     company-dabbrev
  ;;                     company-files
  ;;                     company-yasnippet))
  ;;       (setq-local company-idle-delay 0.1))
  ;;     (map! :local
  ;;           :desc "ob-ipython-inspect" :n "C-S-k" #'ob-ipython-inspect
  ;;           :desc "ob ipython completion" :i "C-n" #'company-ob-ipython)
  ;;     (when (featurep 'lpy)
  ;;       (setq lispy-python-proc
  ;;             (format "Python:ob-ipython-%s"
  ;;                     (ob-ipython--normalize-session
  ;;                      (cdr (assoc :session (nth 2 info)))))
  ;;             lispy--python-middleware-loaded-p nil)
  ;;       (lispy--python-middleware-load))
  ;;     )
  ;;     ;; 4. fix repl completion? company-capf seems to work inconsistently (C-x C-o)
  ;;     ;;    python-shell-completion-complete-or-indent seems to work (TAB = C-i)
  ;;     ;;    but is annoying when there are multiple choices
  ;;   (advice-add '+org*org-babel-edit-prep:ipython :override #'+org*org-babel-edit-prep:ipython-complete)

  ;;   ;; from https://github.com/gregsexton/ob-ipython/issues/135#issuecomment-397463174
  ;;   ;; (advice-add 'ob-ipython--collect-json :before
  ;;   ;;             (lambda (&rest args)
  ;;   ;;               (when (re-search-forward "{" nil t)
  ;;   ;;                 (backward-char))))
  ;;   )

  ; don't want return to execute src blocks
  ; since function is autoloaded we override it with an advice
   (defun +org/dwim-at-point-no-src-execute ()
    "Do-what-I-mean at point.

  If on a:
  - checkbox list item or todo heading: toggle it.
  - clock: update its time.
  - headline: toggle latex fragments and inline images underneath.
  - footnote reference: jump to the footnote's definition
  - footnote definition: jump to the first reference of this footnote
  - table-row or a TBLFM: recalculate the table's formulas
  - table-cell: clear it and go into insert mode. If this is a formula cell,
    recaluclate it instead.
  - babel-call: execute the source block
  - statistics-cookie: update it.
  - latex fragment: toggle it.
  - link: follow it
  - otherwise, refresh all inline images in current tree."
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (`headline
         (cond ((org-element-property :todo-type context)
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               (t
                (+org/refresh-inline-images)
                (org-remove-latex-fragment-image-overlays)
                (org-toggle-latex-fragment '(4)))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies nil)))

        ;((or `src-block `inline-src-block)
        ; (org-babel-execute-src-block))

        ((or `latex-fragment `latex-environment)
         (org-toggle-latex-fragment))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org/refresh-inline-images)
             (org-open-at-point))))

        (_ (+org/refresh-inline-images)))))

    (advice-add '+org/dwim-at-point :override #'+org/dwim-at-point-no-src-execute)

  )                                    

; (setenv "PYTHONPATH" "/home/john/scripts/awful_pip_prefix_thing/lib/python3.6/site-packages/:/home/john/scripts/pyMods/:")
; (setq conda-anaconda-home "/home/john/.pyenv/versions/anaconda3-4.4.0")


;; key binds
(map! :leader
      (:desc "App" :prefix "a"
        :desc "Ielm" :n "i" #'ielm
        :desc "elfeed" :n "e" #'elfeed
        ;; :desc "Mail" :n "m" #'=email
        :desc "Mail" :n "m" #'mu4e
        :desc "Processes" :n "p" #'list-processes
        :desc "Jupyter-repl" :n "j" #'jupyter-run-repl
        ; :desc "External Termite" :n "t" #'open-termite
        :desc "External Iterm" :n "t" #'open-iterm
        :desc "External Ranger" :n "r" #'open-ranger)
      :prefix "m" :desc "schedule" :n "s" #'org-schedule)

(map! :leader
      :mode process-menu-mode
      :desc "Kill process" :n "k" #'process-menu-delete-process)
(map! :mode process-menu-mode
      :desc "Quit" :n "q" (lambda () (interactive)
                            (kill-this-buffer) (evil-quit)))
(map! :desc "store(grab) link" "C-c C-g" #'org-store-link)

(defun copy-window ()
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos))
  )

(map!
 :desc "open copy of current window" :m "go" 'copy-window)

(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len  (if (consp len) fill-column (prefix-numeric-value len)))
  (let ((start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (interactive-p)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      (goto-line start-line)
      (message "Not found"))))

;; Now it reads whatever the version in that dir is, which is a better practice anyway
;; So I will not se pyenv versions here
;; (doom-modeline-mode) ; hack to make pyenv-mode-set not have issues?
;; (pyenv-mode-set "anaconda3-4.4.0")
;;
;;
;;

(if (featurep! :private frames-only)
    (setq org-src-window-setup 'other-frame) ;; other-window doesn't close as I'd like on exit
  (setq org-src-window-setup 'other-window)
  (after!  persp-mode
    (setq persp-interactive-init-frame-behaviour-override -1
          persp-emacsclient-init-frame-behaviour-override -1))
  )

;; fixing where this was broken used to use evil-write instead of save-buffer
;; it used to be that I could just redefine here, but that seems to not work
;; renaming my functiona and putting in as advice instead
; (defun evil-org-edit-src-exit ()
(defun replace-evil-org-edit-src-exit ()
  "Save then `evil-edit-src-exit'."
  (interactive)
  (mapc #'call-interactively '(save-buffer org-edit-src-exit)))
(advice-add 'evil-org-edit-src-exit :override
            'replace-evil-org-edit-src-exit)


;; risky patch hack???
;;; (defun list-processes--refresh ()
;;;   "Recompute the list of processes for the Process List buffer.
;;; Also, delete any process that is exited or signaled."
;;;   (setq tabulated-list-entries nil)
;;;   (dolist (p (process-list))
;;;     (cond ((memq (process-status p) '(exit signal closed))
;;;            (delete-process p))
;;;           ((or (not process-menu-query-only)
;;;                (process-query-on-exit-flag p))
;;;            (let* ((buf (process-buffer p))
;;;                   (type (process-type p))
;;;                   (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
;;;                   (name (process-name p))
;;;                   (status (symbol-name (process-status p)))
;;;                   (buf-label (if (buffer-live-p buf)
;;;                                  `(,(buffer-name buf)
;;;                                    face link
;;;                                    help-echo ,(format-message
;;;                                                "Visit buffer `%s'"
;;;                                                (buffer-name buf))
;;;                                    follow-link t
;;;                                    process-buffer ,buf
;;;                                    action process-menu-visit-buffer)
;;;                                "--"))
;;;                   (tty (or (process-tty-name p) "--"))
;;;                   (cmd
;;;                    (if (memq type '(network serial))
;;;                        (let ((contact (process-contact p t)))
;;;                          (if (eq type 'network)
;;;                              (format "(%s %s)"
;;;                                      (if (plist-get contact :type)
;;;                                          "datagram"
;;;                                        "network")
;;;                                      (if (plist-get contact :server)
;;;                                          (format "server on %s"
;;;                                                  (or
;;;                                                   (plist-get contact :host)
;;;                                                   (plist-get contact :local)))
;;;                                        (format "connection to %s"
;;;                                                (plist-get contact :host))))
;;;                            (format "(serial port %s%s)"
;;;                                    (or (plist-get contact :port) "?")
;;;                                    (let ((speed (plist-get contact :speed)))
;;;                                      (if speed
;;;                                          (format " at %s b/s" speed)
;;;                                        "")))))
;;;                      ;; ACTUAL CHANGE HERE
;;;                      ;; (mapconcat 'identity (process-command p) " "))))
;;;                      (if (not (stringp (process-command p))) ""
;;;                        (mapconcat 'identity (process-command p) " ")))))
;;;              (push (list p (vector name pid status buf-label tty cmd))
;;;                    tabulated-list-entries)))))
;;;   (tabulated-list-init-header))



;; Now with mac osx version, maybe
;;
(defun list-processes--refresh ()
  "Recompute the list of processes for the Process List buffer.
Also, delete any process that is exited or signaled."
  (setq tabulated-list-entries nil)
  (dolist (p (process-list))
    (cond ((memq (process-status p) '(exit signal closed))
           (delete-process p))
          ((or (not process-menu-query-only)
               (process-query-on-exit-flag p))
           (let* ((buf (process-buffer p))
                  (type (process-type p))
                  (pid  (if (process-id p) (format "%d" (process-id p)) "--"))
                  (name (process-name p))
                  (status (symbol-name (process-status p)))
                  (buf-label (if (buffer-live-p buf)
                                 `(,(buffer-name buf)
                                   face link
                                   help-echo ,(format-message
                                               "Visit buffer `%s'"
                                               (buffer-name buf))
                                   follow-link t
                                   process-buffer ,buf
                                   action process-menu-visit-buffer)
                               "--"))
                  (tty (or (process-tty-name p) "--"))
                  (thread
                   (cond
                    ((or
                      (null (process-thread p))
                      (not (fboundp 'thread-name))) "--")
                    ((eq (process-thread p) main-thread) "Main")
                    ((thread-name (process-thread p)))
                    (t "--")))
                  (cmd
                   (if (memq type '(network serial))
                       (let ((contact (process-contact p t t)))
                         (if (eq type 'network)
                             (format "(%s %s)"
                                     (if (plist-get contact :type)
                                         "datagram"
                                       "network")
                                     (if (plist-get contact :server)
                                         (format
                                          "server on %s"
                                          (if (plist-get contact :host)
                                              (format "%s:%s"
                                                      (plist-get contact :host)
                                                      (plist-get
                                                       contact :service))
                                            (plist-get contact :local)))
                                       (format "connection to %s:%s"
                                               (plist-get contact :host)
                                               (plist-get contact :service))))
                           (format "(serial port %s%s)"
                                   (or (plist-get contact :port) "?")
                                   (let ((speed (plist-get contact :speed)))
                                     (if speed
                                         (format " at %s b/s" speed)
                                       "")))))
                     ;;(mapconcat 'identity (process-command p) " "))))
                     (if (not (stringp (process-command p))) ""
                       mapconcat 'identity (process-command p) " "))))
             (push (list p (vector name pid status buf-label tty thread cmd))
   tabulated-list-entries)))))
  (tabulated-list-init-header))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-ref-pdf-directory . "/Documents/papers/pto_divacancies/")
     (org-ref-pdf-directory . "~/Documents/papers/wannier_pol/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
