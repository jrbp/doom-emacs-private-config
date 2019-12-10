;;; private/org-ref/config.el -*- lexical-binding: t; -*-
(def-package! org-ref
  :config
  (require 'org-ref)
  (setq reftex-default-bibliography '("~/org/references/misc/references.bib"))
  ;;
  ;; see org-ref for use of these variables
  (setq org-ref-default-bibliography '("~/org/references/misc/references.bib")
        org-ref-pdf-directory "~/Documents/papers/misc/"
        org-ref-bibliography-notes "~/org/references/misc/notes.org")
  (setq bibtex-completion-bibliography "~/org/references/misc/references.bib"
        bibtex-completion-library-path "~/Documents/papers/misc/"
        bibtex-completion-notes-path "~/org/references/misc/notes.org")
  ;;
  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function 'org-open-file)

  )
