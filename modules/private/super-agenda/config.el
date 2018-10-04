;;; private/frames-only/config.el -*- lexical-binding: t; -*-

(def-package! org-super-agenda
  :config
  ;(setq org-super-agenda-mode 1) how to enable mode?
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
         '((:auto-category t))))
