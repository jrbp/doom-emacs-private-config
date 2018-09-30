;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(after! org
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window))
