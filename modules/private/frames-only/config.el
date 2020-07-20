;;; private/frames-only/config.el -*- lexical-binding: t; -*-

;;(def-package! frames-only-mode ;; def-package! renamed to use-package!
(use-package! frames-only-mode
  :config
  ;(require 'frames-only-mode)
  (frames-only-mode) ;; for now we just enable it after loading (maybe in the future we'll bind something to toggle it)

  ;; Below worked in main config, but seems to be reset if set here?
  ;; (setq org-agenda-window-setup 'other-frame)
  ;; (setq org-src-window-setup 'other-frame)
  ;; (after!  persp-mode
  ;;   (setq persp-interactive-init-frame-behaviour-override -1
  ;;         persp-emacsclient-init-frame-behaviour-override -1))

  (map! (:after helm
          (:after helm-files
            :map (helm-find-files-map helm-read-file-map)
            [M-return] #'helm-ff-run-switch-other-frame)
          (:after helm-locate
            :map helm-generic-files-map
            [M-return] #'helm-ff-run-switch-other-frame)
          (:after helm-buffers
            :map helm-buffer-map
            [M-return] #'helm-buffer-switch-other-frame)
          (:after helm-regexp
            :map helm-moccur-map
            [M-return] #'helm-moccur-run-goto-line-of)
          (:after helm-grep
            :map helm-grep-map
            [M-return] #'helm-grep-run-other-frame-action)))
  )
