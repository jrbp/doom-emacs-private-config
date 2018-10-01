;;; private/org-dnd/config.el -*- lexical-binding: t; -*-

(def-package! ox-dnd
  :config
  ; allow for dndlongtable (could have just set it in :init instead of replacing)
  (setf (cadr (assoc "dnd" org-latex-classes))
        "\\documentclass[letterpaper,10pt,twoside,twocolumn,openany]{book}
[NO-DEFAULT-PACKAGES]
\\usepackage[AUTO]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage{dnd}
\% dndlongtable from https://gist.github.com/benwebber/26e8567380c9f4e291b12a8da3c651c8
\\usepackage{xtab}
\\makeatletter
\\NewDocumentEnvironment{dndlongtable}{O{>{\\centering}p{0.1\\linewidth} p{0.8\\linewidth}} O{tablecolor}}{
  \\par\\vspace*{8pt}
  \\noindent
  \\dnd@TableBodyFont
  \\rowcolors{1}{}{#2}
  \\xentrystretch{-0.1}
  \\begin{xtabular*}{\\linewidth}{#1}
}{
  \\end{xtabular*}
  \\vspace{8pt plus 1pt}
  \\noindent
}
\\makeatother")

  ; redefine, now with more than 2 columns, and allow for dndlongtable
  (defun org-dnd-table (table contents info)
    "Transcode a table from Org to a D&D LaTeX table.
  CONTENTS holds the contents of the table.  INFO is a plist holding
  contextual information."
    (let ((header (first (org-element-property :header table)))
          (align (org-export-read-attribute :attr_dnd table :align))
          (long (org-export-read-attribute :attr_dnd table :long))
          (color (org-export-read-attribute :attr_dnd table :color)))
      (format
       "%s%s"
       (if header (format "\\header{%s}\n" header) "")
       (replace-regexp-in-string
        "begin{tabular.*"
        (format "begin{%s}%s%s"
                (format "%s" (if long "dndlongtable" "dndtable"))
                (if align (format "[%s]" align) (format "[%s]" (org-latex--align-string table info)))
                (if color (format "[%s]" color) "")
                )
        (replace-regexp-in-string
         "end{tabular}"
         (format "end{%s}" (if long "dndlongtable" "dndtable"))
         (replace-regexp-in-string
          "{table}"
          "{table*}"
          (replace-regexp-in-string
           "\\\\\\(begin\\|end\\){center}\n?"
           ""
            (replace-regexp-in-string
             "\\\\centering"
             ""
             (org-latex-table table contents info)))))))));)

  ; update table values on export
  (advice-add 'org-dnd-export-to-pdf :before
              (lambda (&rest r) (org-table-iterate-buffer-tables)))
  )
