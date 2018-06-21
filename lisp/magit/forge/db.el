;;; magit/forge/db.el ---                         -*- lexical-binding: t -*-

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

(require 'closql)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite)

;;; Options

(defcustom magit-forge-database-file "~/.emacs.d/magit-forge-database.sqlite"
  "The file used to store the forge database."
  :package-version '(magit . "2.90.0")
  :group 'magit-forge
  :type 'file)

;;; Commands

;;; Core

(defclass magit-database (closql-database)
  ((object-class :initform magit-forge-project)))

(defvar magit--db-connection nil
  "The EmacSQL database connection.")

(defun magit-db ()
  (unless (and magit--db-connection (emacsql-live-p magit--db-connection))
    (make-directory (file-name-directory magit-forge-database-file) t)
    (closql-db 'magit-database 'magit--db-connection
               magit-forge-database-file t))
  magit--db-connection)

;;; Api

(defun magit-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (magit-db) (apply #'format sql args))
    (apply #'emacsql (magit-db) sql args)))

;;; Schemata

(defconst magit--db-table-schemata
  '((project [(class :not-null)
              (id :not-null :primary-key)
              forge
              owner
              name
              apihost
              githost
              remote
              ;; The schema dsl doesn't properly handle
              ;; `:default', so `closql--db-init' adds
              ;; these columns using `:alter-table'.
              ;; (issues   :default eieio-unbound)
              ;; (pullreqs :default eieio-unbound)
              ])
    (issue   [(class :not-null)
              (id :not-null :primary-key)
              project
              number
              title
              body
              updated
              ]
             (:foreign-key
              [project] :references project [id]
              :on-delete :cascade))
    (pullreq [(class :not-null)
              (id :not-null :primary-key)
              project
              number
              title
              body
              updated
              editable-p
              cross-repo-p
              base-ref
              base-repo
              head-ref
              head-user
              head-repo
              ]
             (:foreign-key
              [project] :references project [id]
              :on-delete :cascade))
    ))

(cl-defmethod closql--db-init ((db magit-database))
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) magit--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (let ((add-column [:alter-table project :add-column $i1 :default $s2]))
      (emacsql db add-column 'issues   'eieio-unbound)
      (emacsql db add-column 'pullreqs 'eieio-unbound))))

;;; _
(provide 'magit/forge/db)
;;; magit/forge/db.el ends here
