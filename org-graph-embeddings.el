;;; org-graph-embeddings.el --- Semantic search for org mode  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 20th February 2023
;; Keywords: outlines

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'plz)

;; Define dot-product helpers in Elisp as fallback if dynamic
;; module is not available.
(unless (and (fboundp 'module-load)
             (module-load (expand-file-name "dotproduct.so")))

  (defun org-graph-dot (a b)
    (let ((len (length a))
          (re 0)
          (i 0))
      (while (< i len)
        (setq re (+ re (* (aref a i) (aref b i))))
        (setq i (1+ i)))
      re))

  (defun org-graph-cosine-similarity (a b)
    (/ (org-graph-dot a b)
       (* (sqrt (org-graph-dot a a))
          (sqrt (org-graph-dot b b)))))

  (defun org-graph-compute-score (query embeddings)
    "Compute score of QUERY against EMBEDDINGS.

QUERY should be a `vector' with query embedding.

EMBEDDINGS should be a vector of vectors of embeddings against
which we compare."
    (let ((i 0))
      (mapcar
       (lambda (emb)
         (cl-incf i)
         (message "Comparing embeddings %d" i)
         (org-graph-cosine-similarity query emb))
       (append embeddings nil)))))

(defvar org-graph-embeddings nil
  "Cached embeddings for org graph headers.")

(defun org-graph-load-embeddings ()
  "Load cached embeddings into Emacs."
  (interactive)
  (setq org-graph-embeddings nil)
  (let ((i 0))
    (f-files
     org-graph-embeddings-cache-dir
     (lambda (file)
       (cl-incf i)
       (message "Loading embedding %d" i)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (let ((json-data (json-read)))
           (push
            (list
             :id (f-no-ext (f-filename file))
             :sha1 (cdr (assq 'sha1 json-data))
             :embedding (cdr (assq 'embedding json-data)))
            org-graph-embeddings)))))))

(defun org-graph-sort-embeddings (query embeddings)
  (-sort (lambda (a b)
           (< 0 (- (plist-get a :score) (plist-get b :score))))
         (let ((scores (org-graph-compute-score
                        query
                        (apply #'vector
                               (--map (plist-get it :embedding)
                                      embeddings)))))
           (-zip-with
            (lambda (score embedding)
              (list :id (plist-get embedding :id) :score score))
            (append scores nil)
            embeddings))))

(defun org-graph-openai-query (query)
  "Embed QUERY using openai embedding API and compare against org graph headlines."
  (interactive "sQuery: ")
  (message "Calling openai API to compute embedding for query...")
  (let* ((data (plz 'post "https://api.openai.com/v1/embeddings"
                 :headers `(("Authorization" . ,(format "Bearer %s" (getenv "OPENAI_TOKEN")))
                            ("Content-Type" . "application/json"))
                 :body (json-serialize
                        `(:model "text-embedding-ada-002" :input ,query))
                 :as 'json-read))
         (embedding (cdr (assq 'embedding (elt (cdr (assq 'data data)) 0)))))
    (pop-to-buffer
     (with-current-buffer (get-buffer-create "org-graph semantic search")
       (erase-buffer)
       (mapcar
        (lambda (x)
          (let ((node (org-graph--make-node (org-id-find (plist-get x :id) t))))
            (insert (format
                     "[%s] %.2f %s\n"
                     (substring-no-properties (plist-get node :id))
                     (* (plist-get x :score) 100)
                     (substring-no-properties (plist-get node :name))))))
        (-take 40 (org-graph-sort-embeddings embedding org-graph-embeddings)))
       (goto-char (point-min))
       (current-buffer)))))

(defun org-graph--extract-content-for-embedding ()
  "Extract headline content for embedding"
  (let ((headlines (org-graph-get-parents nil t))
        (content nil)
        (self (org-graph--make-node (point))))
    (setq content (if (org-graph-leaf-p)
                      (-let* (((_ (&plist :robust-begin :contents-begin :end))
                               (org-element-at-point))
                              (content (buffer-substring-no-properties
                                        (or robust-begin contents-begin) end)))
                        (with-temp-buffer
                          (insert content)
                          (goto-char (point-min))
                          (pop-to-buffer (current-buffer))
                          (ignore-errors (org-graph--forward-to-entry-content))
                          (buffer-substring-no-properties (point) (point-max))))
                    (org-graph--get-headline-content)))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; remove :PROPERTIES: drawers
      (while (re-search-forward ":PROPERTIES:" nil t)
        (let ((beg (match-beginning 0))
              (end (save-excursion
                     (when (re-search-forward ":END:" nil t)
                       (match-end 0)))))
          (when end (delete-region beg end))))
      (setq content (buffer-string)))
    (list
     :id (plist-get self :id)
     :content
     (->> (concat (mapconcat (lambda (h)
                               (substring-no-properties (plist-get h :name)))
                             headlines " ")
                  " "
                  (substring-no-properties (plist-get self :name))
                  " "
                  (or content ""))
          (replace-regexp-in-string " +" " ")
          (replace-regexp-in-string "\\*+" "*")))))

(defun org-graph--get-buffer-content-for-embedding ()
  "Collect embedding content for all headlines in current buffer."
  (let ((headings nil))
    (save-excursion
      (goto-char (point-min))
      (while (outline-next-heading)
        (let ((heading
               (save-excursion
                 (org-graph--extract-content-for-embedding)))
              (level (org-current-level)))
          (push heading headings)
          (when (org-graph-leaf-p)
            (outline-next-heading)
            (while (> (org-current-level) level)
              (outline-next-heading))))))
    headings))

(defun org-graph--queue-embedding-batch-async (queue header-or-headers)
  (let ((inputs (-list header-or-headers)))
    (plz-queue queue
      'post "https://api.openai.com/v1/embeddings"
      :headers `(("Authorization" . ,(format "Bearer %s" (getenv "OPENAI_TOKEN")))
                 ("Content-Type" . "application/json"))
      :body (json-serialize
             `(:model "text-embedding-ada-002"
               :input ,(apply
                        #'vector
                        (--map (with-temp-buffer
                                 (insert (plist-get it :content))
                                 (goto-char (point-min))
                                 (forward-word 3000)
                                 (buffer-substring-no-properties (point-min) (point)))
                               inputs))))
      :as 'json-read
      :then (-lambda ((&alist 'data objects))
              (seq-do
               (-lambda ((&alist 'embedding 'index))
                 (-let (((&plist :id :content) (nth index inputs)))
                   (with-temp-buffer
                     (insert
                      (json-serialize
                       (list
                        :sha1 (secure-hash
                               'sha256
                               content)
                        :embedding embedding)))
                     (shut-up
                       (write-region nil nil
                                     (f-expand
                                      (format "%s.json" id)
                                      org-graph-embeddings-cache-dir)))
                     (message "Saved %s" id))))
               objects))
      :else (lambda (err)
              (message "%s" err)))))

(defun org-graph-compute-embeddings-for-buffer ()
  "Compute embeddings for all headlines in current buffer"
  (interactive)
  (message "Extracting content from buffer ...")
  (let ((output-dir org-graph-embeddings-cache-dir)
        (work-queue (make-plz-queue :limit 5))
        (submitted 0)
        (content (org-graph--get-buffer-content-for-embedding))
        (batch nil))
    (make-directory output-dir t)
    (message "There is %d headers to process" (length content))
    (dolist (header content)
      (let* ((id (plist-get header :id))
             (result-file (f-expand (format "%s.json" id) output-dir)))
        (unless (file-exists-p result-file)
          (message "processing header %s" id)
          (push header batch)
          (cl-incf submitted)
          (when (>= (length batch) 20)
            (org-graph--queue-embedding-batch-async work-queue batch)
            (setq batch nil)))))
    (when batch
      (org-graph--queue-embedding-batch-async work-queue batch))
    (message "Submitted %d headers for processing" submitted)
    (plz-run work-queue)))

(provide 'org-graph-embeddings)
;;; org-graph-embeddings.el ends here
