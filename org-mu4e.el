;;; org-mu4e.el --- Functions to integrate `org-mode' and `mu4e' ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Friday  8 January 2021 15:42:46 IST>
;; Keywords:	org, mu4e, mail
;; Version:     0.1.0

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

;; The code should be considered pre-alpha as a package but the functions are
;; usable

;;; Commentary:
;;
;; Convert a subtree from an `org-mode' buffer to a `mu4e' message and mail it
;; with Google's gmail. Contains utilities to mail the buffer with XOAUTH2
;; through gmail via a python script.

;;; Code:


(require 'dash)
(require 'org)
(require 'org-mime)
(require 'util)


;; from `ref-man'. See https://github.com/akshaybadola/ref-man
(defvar ref-man-public-links-cache)
(defvar org-mu4e-mail-subtree-preprocess-hooks nil
  "Preprocessing hooks to run after inserting contents of org subtree with `org-mu4e-mail-subtree'.")
(defvar org-mu4e-mail-subtree-postprocess-hooks nil
  "Postprocessing hooks to run after inserting contents of org subtree with `org-mu4e-mail-subtree'.")

(defun org-mu4e-remove-useless-items-from-buffer ()
  ;; operate on current buffer
  (let ((state-re (concat "^ *- +State *\"" org-todo-regexp))
        (notes-re "^ *- +\\[note\\]"))
    (util/org-remove-list-items-matching-re-from-buffer
     (string-join (list state-re notes-re) "\\|"))))

(defun org-mu4e-replace-pdf-link-with-gdrive (cache)
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

(defun org-mu4e-insert-urls-from-heading-to-text ()
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
  (goto-char (point-min))
  (when (org-at-heading-p)
    (org-mu4e-insert-urls-from-heading-to-text))
  (while (and (not (eobp)) (outline-next-heading))
    (org-mu4e-insert-urls-from-heading-to-text)))

(defun org-mu4e-convert-pdf-links-to-gdrive ()
  (let ((cache (copy-hash-table ref-man-public-links-cache)))
    (goto-char (point-min))
    (when (org-at-heading-p)
      (condition-case exception
          (org-mu4e-replace-pdf-link-with-gdrive cache)
        (error (warn (nth 1 exception)))))
    (while (and (not (eobp)) (outline-next-heading))
      (condition-case exception
          (org-mu4e-replace-pdf-link-with-gdrive cache)
        (error (warn (nth 1 exception)))))))

(setq org-mu4e-mail-subtree-preprocess-hooks nil)
(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-remove-useless-items-from-buffer)
(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-convert-pdf-links-to-gdrive)
(add-to-list 'org-mu4e-mail-subtree-preprocess-hooks
             #'org-mu4e-insert-urls-in-properties-to-body)
(add-to-list 'org-mu4e-mail-subtree-postprocess-hooks
             #'util/org-remove-all-drawers)

(defun org-mu4e-mime-htmlize (buf-string)
  "Create text/plain + text/html buffer of the current org buffer.
The buffer is inserted into another mail buffer for
replying/forwarding etc. Derived from
`org-mime-org-buffer-htmlize'"
  (interactive)
  (let* ((mail-buf (ido-completing-read "Compose Buffer: "
                                        (mapcar
                                         (lambda (x) (format "%s" x))
                                         (-filter
                                          (lambda (x) (with-current-buffer x
                                                        (not (eq major-mode 'mu4e-compose-mode))))
                                          (buffer-list))))))
    (when mail-buf
      (with-current-buffer mail-buf
        (insert buf-string)
        (org-mime-htmlize)))))

(defun org-mu4e-htmlize-current-buffer ()
  (run-hooks 'org-mu4e-mail-subtree-postprocess-hooks)
  (let ((org-export-with-toc nil)
        (org-export-with-broken-links 'mark)
        (org-export-with-timestamps nil)
        (org-export-with-clocks nil)
        (org-export-with-date nil)
        (org-export-with-properties nil))
    (bury-buffer "*mu4e-org-mail-subtree-buffer*")
    (cond (current-prefix-arg
           (org-mu4e-mime-htmlize (buffer-string)))
          (t (org-mime-org-buffer-htmlize)))))

;; Another implementation with org-mime is given here, though not it's not
;; necessarily for mailing a subtree
;; https://kitchingroup.cheme.cmu.edu/blog/category/email/
(defun org-mu4e-mail-subtree ()
  "Export org subtree with `mu4e'.
The prefix argument is processed by
`org-mu4e-htmlize-current-buffer' and if non-nil then ask for an
existing buffer to insert the file."
  (interactive)
  (when (boundp 'ref-man-public-links-cache)
    (let ((buf-string (save-restriction (org-narrow-to-subtree)
					(buffer-string)))
          (mu4e-export-buf (get-buffer-create "*mu4e-org-mail-subtree-buffer*"))
          (org-export-with-toc nil))
      ;; (setq my/mu4e-org-mail-subtree-buf-string buf-string)
      (with-current-buffer mu4e-export-buf
	(goto-char (point-min))
        (insert buf-string)
        (org-mode)
        (run-hooks 'org-mu4e-mail-subtree-preprocess-hooks)
        (switch-to-buffer mu4e-export-buf)
        (org-show-all)
        (setq-local org-finish-function #'org-mu4e-htmlize-current-buffer)))))

;;; org-mu4e.el ends here
