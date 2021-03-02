;;; org-mu4e.el --- Functions to integrate `org-mode' and `mu4e' ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Friday  8 January 2021 15:42:46 IST>
;; Keywords:	org, mu4e, mail, org-mime
;; Version:     0.2.0

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Convert a buffer (or subtree) from an `org-mode' buffer to a `mu4e' message
;; and mail it.
;;
;; This package contains extra utilities to preprocess and postprocess the org
;; buffer to remove headings and items with a regexp search, convert PDF file
;; URIs to cloud URIs and remove all property drawers.
;;
;; Optionally you can mail the buffer with XOAUTH2 through gmail via a python
;; script also.

;;; Code:

(require 'dash)
(require 'org)
(require 'org-mime)
(require 'util)

(defcustom org-mu4e-links-cache-file ""
  "Text File in `org-mu4e-links-cache' format." ;
  :type 'file
  :group 'org-mu4e)

(defvar org-mu4e-links-cache nil
  "A hash table mapping local files to a remote cache.
Must be defined and maintained by the user.  It should consist
`local-file;remote-file' pairs of lines of texts with a `;'
delimiter")

(defvar org-mu4e-mail-subtree-preprocess-hooks nil
  "Preprocessing hooks to run after inserting contents of org subtree with `org-mu4e-mail-subtree'.")

(defvar org-mu4e-mail-subtree-postprocess-hooks nil
  "Postprocessing hooks to run after inserting contents of org subtree with `org-mu4e-mail-subtree'.")

(defvar org-mu4e-buffer-name "*org-mu4e-mail-subtree-buffer*"
  "Name of the temp buffer for editing the subtree")

(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-remove-useless-items-from-buffer)
(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-convert-pdf-links-to-gdrive)
(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-insert-urls-in-properties-to-body)
(add-to-list 'org-mu4e-mail-subtree-postprocess-hooks
             #'util/org-remove-all-drawers)

(defun org-mu4e-load-links-cache ()
  "Load the existing cache from disk if defined.
See `org-mu4e-links-cache'."
  (interactive)
  (if (string-empty-p org-mu4e-links-cache-file)
      (user-error "File %s is not defined" org-mu4e-links-cache-file)
    (setq org-mu4e-links-cache (make-hash-table :test 'equal))
    (seq-do (lambda (x)
              (let ((split (split-string x ";")))
                (puthash (car split) (cadr split) org-mu4e-links-cache)))
            (split-string (with-current-buffer
                              (find-file-noselect org-mu4e-links-cache-file)
                            (buffer-string)) "\n" t))
    (message "[org-mu4e] Loaded remote links cache from disk.")))

(defun org-mu4e-remove-useless-items-from-buffer ()
  "Remove unwanted items from current `org-mode' buffer.
Uses `util' package to remove items or headings.

This function removes STATE changes and list items beginning with
`[note]'. Items or subtrees matching custom regexps can be
searched and removed.

See `util/org-remove-list-items-matching-re-from-buffer' and
`util/org-remove-subtrees-matching-re'."
  ;; operates on current buffer
  (let ((state-re (concat "^ *- +State *\"" org-todo-regexp))
        (notes-re "^ *- +\\[note\\]"))
    (util/org-remove-list-items-matching-re-from-buffer
     (string-join (list state-re notes-re) "\\|"))))

(defun org-mu4e-replace-pdf-link-with-gdrive (cache)
  "Replace pdf file URIs for current heading with gdrive links.
CACHE is a hash table mapping file URIs to gdrive URIs.

For an example of such a cache, see `ref-man-public-links-cache' in
URL `https://github.com/akshaybadola/ref-man/blob/master/ref-man-remote.el'."
  (when (org-entry-get (point) "PDF_FILE")
    (let* ((pblock (org-get-property-block))
           (file (replace-regexp-in-string
                  "\\[\\|\\]" "" (org-entry-get (point) "PDF_FILE")))
           (link (gethash file cache)))
      (goto-char (cdr pblock))
      (end-of-line)
      (open-line 1)
      (forward-line)
      (indent-relative)
      (unless link
        (error "File %s not in cache" file))
      (insert "- gdrive_link: " link))))

(defun org-mu4e-move-urls-from-property-drawer-to-text ()
  "Move a URL from property drawer in current heading to text."
  (let ((pblock (org-get-property-block)))
    (mapcar (lambda (x) (when (string-match-p "URL$" (car x))
                          (goto-char (cdr pblock))
                          (end-of-line)
                          (open-line 1)
                          (forward-line)
                          (indent-relative)
                          (insert "- " (downcase (car x)) ": " (cdr x))))
            (org-entry-properties))))

(defun org-mu4e-insert-urls-in-properties-to-body ()
  "Move URLs from property drawers to text in current `org-mode' buffer to text."
  (goto-char (point-min))
  (when (org-at-heading-p)
    (org-mu4e-move-urls-from-property-drawer-to-text))
  (while (and (not (eobp)) (outline-next-heading))
    (org-mu4e-move-urls-from-property-drawer-to-text)))

(defun org-mu4e-convert-pdf-links-to-gdrive ()
  "Replace pdf file URIs for entire buffer with gdrive links.
This hook is run only if `org-mu4e-links-cache' is non-nil."
  (when org-mu4e-links-cache
    (let ((cache (copy-hash-table org-mu4e-links-cache)))
      (goto-char (point-min))
      (when (org-at-heading-p)
        (condition-case exception
            (org-mu4e-replace-pdf-link-with-gdrive cache)
          (error (warn (nth 1 exception)))))
      (while (and (not (eobp)) (outline-next-heading))
        (condition-case exception
            (org-mu4e-replace-pdf-link-with-gdrive cache)
          (error (warn (nth 1 exception))))))))

(defun org-mu4e-mime-htmlize (buf-string)
  "Convert org buffer contents to text/plain + text/html to mail.
BUF-STRING is the buffer string from the desired `org-mode'
buffer.  This is inserted into another mail buffer for
replying/forwarding etc.  Derived from
`org-mime-org-buffer-htmlize'"
  (interactive)
  (let* ((mail-buf (ido-completing-read "Compose Buffer: "
                                        (mapcar
                                         (lambda (x) (format "%s" x))
                                         (-filter
                                          (lambda (x) (with-current-buffer x
                                                        (eq major-mode 'mu4e-compose-mode)))
                                          (buffer-list))))))
    (when mail-buf
      (with-current-buffer mail-buf
        (insert buf-string)
        (org-mime-htmlize)))))

(defun org-mu4e-htmlize-current-buffer ()
  "Export current buffer with `org-mime-htmlize'.
Prior to export `org-mu4e-mail-subtree-postprocess-hooks' is run
on the current buffer.

With a single prefix arg `C-u' export to an existing
`mu4e-compose' buffer with `org-mu4e-mime-htmlize'."
  (run-hooks 'org-mu4e-mail-subtree-postprocess-hooks)
  (let ((org-export-with-toc nil)
        (org-export-with-broken-links 'mark)
        (org-export-with-timestamps nil)
        (org-export-with-clocks nil)
        (org-export-with-sub-superscripts nil)
        (org-export-with-date nil)
        (org-export-with-properties nil)
        (buf-string (buffer-string)))
    (bury-buffer)
    (if current-prefix-arg
        (org-mu4e-mime-htmlize buf-string)
      (with-current-buffer org-mu4e-buffer-name
        (org-mime-org-buffer-htmlize)))))

;; Another implementation with `org-mime' is given here, though not it's not
;; necessarily for mailing a subtree
;; https://kitchingroup.cheme.cmu.edu/blog/category/email/
(defun org-mu4e-mail-subtree ()
  "Export org subtree with `org-mime'.
The prefix argument is processed by
`org-mu4e-htmlize-current-buffer' and if non-nil then ask for an
existing `mu4e-compose' buffer to insert the file."
  (interactive)
  (unless org-mu4e-links-cache
    (message "[org-mu4e] %s is nil, proceeding without cache" org-mu4e-links-cache))
  (let ((buf-string (save-restriction (org-narrow-to-subtree)
				      (buffer-string)))
        (mu4e-export-buf (get-buffer-create org-mu4e-buffer-name))
        (org-export-with-toc nil))
    (with-current-buffer mu4e-export-buf
      (goto-char (point-min))
      (insert buf-string)
      (org-mode)
      (run-hooks 'org-mu4e-mail-subtree-preprocess-hooks)
      (switch-to-buffer mu4e-export-buf)
      (org-show-all)
      (setq-local org-finish-function #'org-mu4e-htmlize-current-buffer))))

(provide 'org-mu4e)

;;; org-mu4e.el ends here

