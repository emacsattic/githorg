;;; githorg.el --- Integrate Github Issues & Org-Mode

;; Copyright (c) 2011 Ian Eure <ian.eure@gmail.com>

;; Author: Ian Eure <ian.eure@gmail.com>

;; Keywords: github issues org
;; Last edit: 2011-02-17
;; Version: 0.1

;; githorg.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with your copy of Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; (autoload 'githorg-select "githorg")
;; M-x githorg-select
;;
;;; Code:

(require 'url)
(require 'json)
(eval-when-compile (require 'cl))



(defgroup githorg nil
  "Integrate Github Issues & Org-Mode"
  :prefix "githorg-"
  :group 'tools)

(defconst githorg-github-api
  "https://github.com/api/v2/json/issues")

(defcustom githorg-github-username (user-login-name)
  "Your GitHub username"
  :tag "Github Username"
  :group 'githorg
  :type '(string))

(defcustom githorg-github-token ""
  "Your API token for GitHub. You can find this on https://github.com/account/admin"
  :tag "Github API Token"
  :group 'githorg
  :type '(string))

(defface githorg-patch-button
  '((t (:foreground "DarkGreen" :underline t :weight bold)))
  "The face for buttons which open attached patches."
  :group 'githorg)

(defcustom githorg-patch-button-face 'githorg-patch-button
  "The face to be used for patch buttons."
  :type 'face
  :group 'githorg)

(defvar githorg-patch-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'githorg-open-patch)
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap used by Githorg for patch buttons")

(defvar githorg-last-state)
(make-variable-buffer-local 'githorg-last-state)

(defvar githorg-project)
(make-variable-buffer-local 'githorg-project)



(defun githorg-auth-headers (&optional username token)
  (let ((username (or username githorg-github-username))
        (token (or token githorg-github-token)))
    (cons "Authorization"
          (format "Basic %s"
                  (base64-encode-string (format "%s/token:%s" username token))))))

(defmacro with-githorg-auth (&rest body)
  `(let ((url-request-extra-headers
          '(,(githorg-auth-headers))))
     ,@body))

(defun githorg-fetch-open-issues (project)
  "Return only open issues."
  (cdr (assq 'issues (githorg-fetch project 'list 'open))))

(defun githorg-fetch-closed-issues (project)
  "Return only closed issues."
  (cdr (assq 'issues (githorg-fetch project 'list 'closed))))

(defun githorg-fetch-issues (project)
  "Return all issues."
  (vconcat (githorg-fetch-open-issues project)
           (githorg-fetch-closed-issues project)))

(defun githorg-owner-projectp (project &optional owner)
  (let ((owner (or owner githorg-github-username)))
    (string= (substring project 0 (+ (length owner) 1))
             (concat owner "/"))))

(defun githorg-fetch (project action &optional args)
  "Perform a request to the Issues API. "
  (let* ((url-request-extra-headers
          (when (githorg-owner-projectp project)
            (list (githorg-auth-headers))))
         (action (if (symbolp action) (symbol-name action) action))
         (url (format (concat "%s/%s/%s" (when args "/%s"))
                      githorg-github-api action project args)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n")
      (prog1 (json-read)
        (kill-buffer)))))

;;  Modes

;; These might be better as minor modes

(define-derived-mode githorg-issue-list-mode org-mode
  "Major mode for listing GitHub issues."
  :group 'githorg)

(defun githorg-issues (project)
  "List issues for a GitHub project in a new githorg buffer."
  (interactive (list (read-string "Project: ")))
  (let ((buffer-read-only t))
    (setq githorg-project project)
    (pop-to-buffer (format "*githorg:%s*" project))
    (githorg-issue-list-mode)
    (setq githorg-last-state (githorg-fetch-open-issues project))

    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (githorg-issue-refresh))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun githorg-issue-refresh ()
  "Refresh the display of issues."
  (insert (format "* Issues for [[http://github.com/%s][%s]]\n\n"
                  githorg-project githorg-project))

  (mapc 'githorg-format-issue (append githorg-last-state nil)))

(defun githorg-transform (prefix vars)
  (mapcar (lambda (cell)
            (list (intern (concat prefix (symbol-name (car cell)))) (cdr cell)))
          vars))

(defun githorg-open-patch ()
  (interactive)
  (let ((patch-url (get-text-property (point) 'url))
        (issue-number (get-text-property (point) 'issue-number)))
    (pop-to-buffer (format "*githorg:%s/%s.patch" githorg-project issue-number))
    (when (and (bobp) (eobp))
      (widen)
      (delete-region (point-min) (point-max))
      (insert (with-current-buffer (url-retrieve-synchronously patch-url)
                (search-forward "\n\n")
                (buffer-substring (point) (point-max))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (diff-mode))))

(defun githorg-display-avatar (hash size buffer beg end)
  (lexical-let ((buffer buffer)
                (beg beg)
                (end end))
    (url-retrieve (format "http://www.gravatar.com/avatar/%s?s=%s" hash size)
                  (lambda (&rest ignore)       ;; Callback
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (add-text-properties beg end
                      (list 'invisible nil
                            'display
                            (list 'image
                                  :type 'jpeg
                                  :data (buffer-substring (point) (point-max))
                                  :ascent 'center :background nil))
                      buffer)))))


(defun githorg-format-issue (issue)
  "Format a JSON GitHub issue into text appropriate for org-mode."
  (let* ((issue-start (point))
         (number (cdr (assq 'number issue)))
         (state (cdr (assq 'state issue)))
         (html_url (cdr (assq 'html_url issue)))
         (title (cdr (assq 'title issue)))
         (patch_url (cdr (assq 'patch_url issue)))
         (issue-labels (append (cdr (assq 'labels issue)) nil))
         (label-tags (mapconcat 'identity issue-labels ":"))
         (gravatar_id (cdr (assq 'gravatar_id issue)))
         (user (cdr (assq 'user issue)))
         (body (replace-regexp-in-string "" "" (cdr (assq 'body issue)))))

    (insert "** " (if (string= state "open") "TODO" "DONE")
            (format " [[%s][%s]]" html_url title)
            (if issue-labels (concat " :" label-tags ":") "")

            "\n\n")

    ;; Avatars
    (insert "   ")
    (when (display-images-p)
      (let ((start (point)))
        (insert "[avatar]")
        (githorg-display-avatar gravatar_id 16 (current-buffer)
                                start (line-end-position)))
      (insert " "))
    (insert (concat user ": "))

    ;; Insert & fill body
    (let ((body-start (point))
          (fill-prefix "   "))
      (insert body)
      (indent-region body-start (point))
      (fill-nonuniform-paragraphs body-start (point))
      ;; (add-text-properties body-start (point) (list 'read-only t))
      )

    ;; Patch, if available.
    (insert "\n")
    (when patch_url
      (insert "\n   ")
      (insert "Attached Patch")
      (add-text-properties (line-beginning-position) (point)
                           (list 'keymap githorg-patch-button-keymap
                                 'url patch_url
                                 'face githorg-patch-button-face
                                 ;; 'read-only t
                                 ))
      (insert "\n"))
    (insert "\n")

    (add-text-properties issue-start (point)
                         (list 'issue-number number))))



;; (define-derived-mode 'githorg-issue-mode org-mode
;;   "Major mode for interacting with GitHub issues.")

(provide 'githorg)
