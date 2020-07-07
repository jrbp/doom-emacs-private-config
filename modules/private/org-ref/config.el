;;; private/org-ref/config.el -*- lexical-binding: t; -*-
;;(def-package! org-ref ;def-package! renamed to use-package!
(use-package! org-ref
  :after org
  :config
  (setq reftex-default-bibliography '("~/org/references/misc/references.bib"))
  ;;
  ;; see org-ref for use of these variables
  (setq org-ref-default-bibliography '("~/org/references/misc/references.bib")
        org-ref-pdf-directory "/home/john/Documents/papers/unsorted/"
        org-ref-bibliography-notes "~/org/references/misc/notes.org")
  (setq bibtex-completion-bibliography "~/org/references/misc/references.bib"
        bibtex-completion-library-path "~/Documents/papers/unsorted/"
        bibtex-completion-notes-path "~/org/references/misc/notes.org")

  (after! bibtex
    (bibtex-set-dialect 'BibTeX))

  (defun org-ref-specified-pdf-to-bibtex ()
    "Add pdf to bib file and save pdf to `org-ref-default-bibliography'. "
    (interactive)
    (let* ((file (read-file-name "Select file associated with entry: " "~/Downloads/"))
           (dois (org-ref-extract-doi-from-pdf file))
           (doi-utils-download-pdf nil)
           (doi (if (= 1 (length dois))
                    (car dois)
                  (completing-read "Select DOI: " dois))))
    ;; Add bib entry from doi:
      (doi-utils-add-bibtex-entry-from-doi doi)
    ;; Copy pdf to `org-ref-pdf-directory':
      (let ((key (org-ref-bibtex-key-from-doi doi (buffer-file-name))))
        (funcall org-ref-pdf-to-bibtex-function
                 file
                 (expand-file-name (format "%s.pdf" key)
                                   org-ref-pdf-directory)))))
    ;;
  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function 'org-open-file)

  (map!
   :after latex
   :desc "Insert citation from bib"
   :map LaTeX-mode-map
   "C-c l" #'org-ref-helm-insert-cite-link
   "C-c k" #'org-ref-latex-click)

  (map!
   :after bibtex
   :desc "org ref hydra"
   :map bibtex-mode-map
   "C-k" #'org-ref-bibtex-hydra/body)
  )
