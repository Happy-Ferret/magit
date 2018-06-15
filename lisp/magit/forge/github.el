;;; magit/forge/github.el ---                     -*- lexical-binding: t -*-

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

(require 'ghub)
(require 'magit/forge)
(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

;;; Variables

(defvar magit-github-token-scopes '(repo)
  "The Github API scopes needed by Magit.

`repo' is the only required scope.  Without this scope none of
Magit's features that use the API work.  Instead of this scope
you could use `public_repo' if you are only interested in public
repositories.

`repo' Grants read/write access to code, commit statuses,
  invitations, collaborators, adding team memberships, and
  deployment statuses for public and private repositories
  and organizations.

`public_repo' Grants read/write access to code, commit statuses,
  collaborators, and deployment statuses for public repositories
  and organizations. Also required for starring public
  repositories.")

;;; Projects

(defclass magit-github-project (magit-forge-project)
  ())

(cl-defmethod magit-forge--object-id
  ((_class (subclass magit-github-project)) forge host owner name)
  "Return the id of the specified project.
This method has to make an API request."
  (let-alist (ghub-graphql "\
query ($owner:String!, $name:String!) {
  repository(owner:$owner, name:$name) {
    id }}" `((owner . ,owner)
             (name  . ,name))
    :host host :auth 'magit)
    (format "%s:%s" forge .data.repository.id)))

;;; Issues

(cl-defmethod magit-forge--pull-issues ((prj magit-github-project))
  (emacsql-with-transaction (magit-db)
    (mapc #'closql-delete (oref prj issues))
    (dolist (i (magit-forge--fetch-issues prj))
      (let ((issue (let-alist i
                     (magit-forge-issue
                      :id      (magit-forge--object-id prj .number)
                      :project (oref prj id)
                      :number  .number
                      :title   .title
                      :body    .body
                      :updated .updatedAt
                      ))))
        (closql-insert (magit-db) issue)))))

(defconst magit-github--fetch-issues "\
query ($owner:String!, $name:String!) {
  repository(owner:$owner, name:$name) {
    issues(first:100, states:[OPEN]) {
      totalCount
      pageInfo {
        startCursor
        endCursor
        hasNextPage }
      edges {
        cursor
        node {
          number
          updatedAt
          title }}}}}")

(cl-defmethod magit-forge--fetch-issues ((prj magit-github-project))
  (--map (cdr (assq 'node it))
         (let-alist (magit--ghub-graphql prj magit-github--fetch-issues
                                         `((owner . ,(oref prj owner))
                                           (name  . ,(oref prj name))))
           .data.repository.issues.edges)))

;;; Pullreqs

(cl-defmethod magit-forge--pull-pullreqs ((prj magit-github-project))
  (emacsql-with-transaction (magit-db)
    (mapc #'closql-delete (oref prj pullreqs))
    (dolist (p (magit-forge--fetch-pullreqs prj))
      (let ((pullreq (let-alist p
                       (magit-forge-pullreq
                        :id           (magit-forge--object-id prj .number)
                        :project      (oref prj id)
                        :number       .number
                        :title        .title
                        :body         .body
                        :updated      .updatedAt
                        :editable-p   .maintainerCanModify
                        :cross-repo-p .isCrossRepository
                        :base-ref     .baseRef.name
                        :base-repo    .baseRef.repository.nameWithOwner
                        :head-ref     .headRef.name
                        :head-user    .headRef.repository.owner.login
                        :head-repo    .headRef.repository.nameWithOwner
                        ))))
        (closql-insert (magit-db) pullreq)))))

(defconst magit-github--fetch-pullreqs "\
query ($owner:String!, $name:String!) {
  repository(owner:$owner, name:$name) {
    pullRequests(first:100, states:[OPEN]) {
      totalCount
      pageInfo {
        startCursor
        endCursor
        hasNextPage }
      edges {
        cursor
        node {
          baseRef {
            name
            repository {
              nameWithOwner }}
          body
          headRef {
            name
            repository {
              owner {
                login }
              nameWithOwner }}
          isCrossRepository
          maintainerCanModify
          number
          state
          title
          updatedAt }}}}}")

(cl-defmethod magit-forge--fetch-pullreqs ((prj magit-github-project))
  (--map (cdr (assq 'node it))
         (let-alist (magit--ghub-graphql prj magit-github--fetch-pullreqs
                                         `((owner . ,(oref prj owner))
                                           (name  . ,(oref prj name))))
           .data.repository.pullRequests.edges)))

;;; Utilities

(cl-defun magit--ghub-graphql (prj graphql &optional variables
                                   &key silent callback errorback)
  (ghub-graphql graphql variables
                :host      (oref prj apihost)
                :auth      'magit
                :silent    silent
                :callback  callback
                :errorback errorback))

;;; _
(provide 'magit/forge/github)
;;; magit/forge/github.el ends here
