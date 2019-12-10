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


(defun open-termite ()
  (interactive)
  (call-process-shell-command "termite&" nil 0))

(defun open-ranger ()
  (interactive)
  (call-process-shell-command "termite -e ranger&" nil 0))

; terrible hack for sshfs files to not be super slow
(defun open-as-sym-link (f)
  (interactive "fFile:")
  (let ((sl
        (concat
         (file-name-as-directory "/home/john/.tmp_sshfs_symlinks")
         (file-name-nondirectory f))))
    (if (not (file-exists-p sl))
        (f-symlink f sl))
    (switch-to-buffer (find-file sl))))

(after! mu4e
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
 ; doom doesn't load org-mu4e until composing
 ; this stops us from using its org-capture function
  (require 'org-mu4e nil 'noerror)
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
  ; (add-hook! 'org-mode-hook (setq-local display-line-numbers 'nil)) ; for newer version of doom i reverted from
  ; org was getting slow, disabling some things for speed up here:
  (remove-hook! 'org-mode-hook #'+org|enable-auto-update-cookies)
  (advice-remove #'evil-org-open-below #'+org*evil-org-open-below) ; didn't like this anyway
  ; realzied there were some bad json dumps in some files, cleaning up the really long lines helped a lot
  ; see goto-long-line which has been added to this config
  ;
  ; I didn't like the auto completeion of the formatting stuff
  (sp-with-modes 'org-mode
    (sp-local-pair "*" nil :actions :rem)
    (sp-local-pair "_" nil :actions :rem)
    (sp-local-pair "/" nil :actions :rem)
    (sp-local-pair "~" nil :actions :rem)
    (sp-local-pair "=" nil :actions :rem))


  ; Didn't like that new headings on C-return weren't put in at point
  (setq org-insert-heading-respect-content nil)
  (map! :map evil-org-mode-map
        :desc "New header at point"
        :ni [C-return] 'org-insert-heading)

  (add-to-list 'org-file-apps '("\\.vesta\\'" . "VESTA %s"))
  (add-to-list 'org-file-apps '("\\.nb\\'" . "mathematica %s"))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
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
         ("a" "Appointments" entry (file+headline "~/org/master.org" "Misc appointments")
          "* %?\n  %i %a %U")
         ("n" "Notes" entry (file+headline "~/org/master.org" "Notes")
          "* %?\n  %i %a %U")
         ))


  (after! ob-ipython
    ;; made ob-ipython work for newer version of doom I reverted from
    ;; (after! ob-async
    ;;   (add-to-list 'ob-async-no-async-languages-alist "ipython"))

    (setq ob-ipython-command "/home/john/.pyenv/shims/jupyter")
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

    ;; from https://github.com/gregsexton/ob-ipython/issues/135#issuecomment-397463174
    ;; (advice-add 'ob-ipython--collect-json :before
    ;;             (lambda (&rest args)
    ;;               (when (re-search-forward "{" nil t)
    ;;                 (backward-char))))
    )

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

(pyenv-mode-set "anaconda3-4.4.0")
(setenv "PYTHONPATH" "/home/john/scripts/awful_pip_prefix_thing/lib/python3.6/site-packages/:/home/john/scripts/pyMods/:")
; (setq conda-anaconda-home "/home/john/.pyenv/versions/anaconda3-4.4.0")

(when (featurep! :private frames-only)
  (after!  persp-mode
    (setq persp-interactive-init-frame-behaviour-override -1
          persp-emacsclient-init-frame-behaviour-override -1)))

;; key binds
(map! :leader
      (:desc "App" :prefix "a"
        :desc "Ielm" :n "i" #'ielm
        :desc "Mail" :n "m" #'=email
        :desc "Processes" :n "p" #'list-processes
        :desc "External Termite" :n "t" #'open-termite
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
