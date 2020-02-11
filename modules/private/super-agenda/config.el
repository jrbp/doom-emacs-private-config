;;; private/frames-only/config.el -*- lexical-binding: t; -*-

;;(def-package! org-super-agenda ;def-package! renamed to use-package!
(use-package! org-super-agenda
  :config
  ; following defun makes headers dumb, but at least doesn't break evil bindings
  (defun org-super-agenda--make-agenda-header (s)
    "Return agenda header containing string S.
Prepended with `org-super-agenda-header-separator'."
    (pcase s
      ('none "")
      (_ (setq s (concat " " s))
         (org-add-props s nil 'face 'org-agenda-structure)
                        ;'keymap org-super-agenda-header-map
                        ;;; NOTE: According to the manual, only `keymap' should be necessary, but in my
                        ;;; testing, it only takes effect in Agenda buffers when `local-map' is set, so
                        ;;; we'll use both.
                        ;'local-map org-super-agenda-header-map)
         (concat org-super-agenda-header-separator s))))
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
         '((:name "Appointments"
                  :time-grid t)
           (:name "Important"
                  :priority "A")
           (:name "At some point"
                  :priority "C" :order 99)
           (:auto-category t)
           )))
