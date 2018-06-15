;;; magit/forge/gitlab.el ---                     -*- lexical-binding: t -*-

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

(require 'glab)
(require 'magit/forge)
(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

;;; Variables

(defvar magit-gitlab-token-scopes '(api)
  "The Gitlab API scopes needed by Magit.

`api' is the only required scope.  It gives read and write access
to everything.  The Gitlab API provides more fine-grained scopes
for read-only access, but when any write access is required, then
it is all or nothing.")

;;; Projects

(defclass magit-gitlab-project (magit-forge-project)
  ((issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   (pullreq-url-format        :initform "https://%h/%o/%n/merge_requests/%i")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/merge_requests/new")))

(cl-defmethod magit-forge--object-id
  ((_class (subclass magit-gitlab-project)) forge host owner name)
  "Return the id of the specified project.
This method has to make an API request."
  (format "%s:%s" forge
          (glab-get (format "/projects/%s%%2F%s" owner name) nil
                    :host host :auth 'magit)))

;;; Issues

(cl-defmethod magit-forge--pull-issues ((prj magit-gitlab-project))
  (emacsql-with-transaction (magit-db)
    (mapc #'closql-delete (oref prj issues))
    (dolist (i (magit-forge--fetch-issues prj))
      (let ((issue (let-alist i
                     (magit-forge-issue
                      :id      (magit-forge--object-id prj .iid)
                      :project (oref prj id)
                      :number  .iid
                      :title   .title
                      :body    t ; TODO
                      :updated .updated_at
                      ))))
        (closql-insert (magit-db) issue)))))

(cl-defmethod magit-forge--fetch-issues ((prj magit-gitlab-project))
  (magit--glab-get prj
                   (format "/projects/%s%%2F%s/issues"
                           (oref prj owner)
                           (oref prj name))
                   `((state . "opened"))))

;;; Pullreqs

(cl-defmethod magit-forge--pull-pullreqs ((prj magit-gitlab-project))
  (emacsql-with-transaction (magit-db)
    (mapc #'closql-delete (oref prj pullreqs))
    (dolist (p (magit-forge--fetch-pullreqs prj))
      (let ((pullreq (let-alist p
                       (magit-forge-pullreq
                        :id           (magit-forge--object-id prj .iid)
                        :project      (oref prj id)
                        :number       .iid
                        :title        .title
                        :body         .description
                        :updated      .updated_at
                        :editable-p   .allow_maintainer_to_push
                        :cross-repo-p (not (equal .source_project_id
                                                  .target_project_id))
                        :base-ref     .target_branch
                        :base-repo    .target_project.path_with_namespace
                        :head-ref     .source_branch
                        :head-user    .source_project.owner.username
                        :head-repo    .source_project.path_with_namespace
                        ))))
        (closql-insert (magit-db) pullreq)))))

(cl-defmethod magit-forge--fetch-pullreqs ((prj magit-gitlab-project))
  (let (target-project)
    (mapcar
     (lambda (alist)
       (let-alist alist
         (setf (alist-get 'source_project alist)
               (magit--glab-get prj (format "/projects/%s" .source_project_id)))
         (setf (alist-get 'target_project alist)
               (or target-project
                   (setq target-project
                         (magit--glab-get prj (format "/projects/%s"
                                                      .target_project_id)))))))
     (magit--glab-get prj (format "/projects/%s%%2F%s/merge_requests"
                                  (oref prj owner)
                                  (oref prj name))
                      `((state . "opened"))))))

;;; Utilities

(cl-defun magit--glab-get (prj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               callback errorback)
  (glab-get resource params
            :host (oref prj apihost)
            :auth 'magit
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

;;; _
(provide 'magit/forge/gitlab)
;;; magit/forge/gitlab.el ends here
