;;; magit/forge/issue.el ---                      -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'magit/forge)

;;; Core

(defclass magit-forge-issue (magit-forge-topic)
  ((closql-table         :initform issue)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform project)
   (closql-foreign-table :initform project)
   (closql-class-prefix  :initform "magit-forge-")
   (id        :initarg :id)
   (project   :initarg :project)
   (number    :initarg :number)
   (title     :initarg :title)
   (body      :initarg :body)
   (updated   :initarg :updated)
   ))

;;; Query

(cl-defmethod magit-issue-get ((prj magit-forge-project) number)
  (closql-get (magit-db)
              (magit-forge--object-id prj number)
              'magit-forge-issue))

(cl-defmethod magit-issue-get ((number integer))
  (when-let (prj (magit-forge-get-project nil))
    (magit-issue-get prj number)))

;;; Utilities

(defun magit-read-issue (prompt)
  (let* ((prj     (magit-forge-get-project t))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref prj issues))
         (default (magit-issue-at-point))
         (choice  (magit-completing-read
                   prompt
                   (mapcar format choices)
                   nil nil nil nil
                   (and default (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (magit-issue-get prj number))))

;;; Sections

(defun magit-issue-at-point ()
  (magit-section-when issue))

(defvar magit-issue-section-map
  (let ((map (make-sparse-keymap)))
    map))

(defun magit-insert-issues ()
  (when-let ((prj (magit-forge-get-project nil))
             (issues (oref prj issues)))
    (magit-insert-section (issues nil t)
      (magit-insert-heading "Issues:")
      (let ((format (format "%%-%is %%s\n"
                            (1+ (length
                                 (format "%i" (oref (car issues) number)))))))
        (dolist (issue issues)
          (with-slots (number title) issue
            (magit-insert-section (issue issue)
              (insert (format format
                              (propertize (format "#%s" number)
                                          'face 'magit-dimmed)
                              title))))))
      (insert ?\n))))

;;; _
(provide 'magit/forge/issue)
;;; magit/forge/issue.el ends here
