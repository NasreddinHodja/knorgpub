;;; build-toc.el --- Generate TOC for org files -*- lexical-binding: t; -*-
;;; Loads config.el from the project root (CWD when invoked).

(require 'org)
(require 'org-element)

;; --- configurable defaults (override in config.el) ---
(defvar knorgpub/content-dir (expand-file-name "content" default-directory))
(defvar knorgpub/public-dir  (expand-file-name "public"  default-directory))
(defvar knorgpub/toc-title   "Table of Contents")

(let ((config (expand-file-name "config.el" default-directory)))
  (unless (file-exists-p config)
    (error "knorgpub: config.el not found in %s - run 'knorgpub init' first" default-directory))
  (load config))

;; --- derived (depend on config) ---
(defvar knorgpub/notes-dir  (file-name-as-directory knorgpub/content-dir))
(defvar knorgpub/index-file (expand-file-name "index.org" knorgpub/notes-dir))

;; --- functions ---

(defun knorgpub/validate-env ()
  (unless (file-directory-p knorgpub/notes-dir)
    (error "Content directory %s does not exist" knorgpub/notes-dir))
  (unless (file-exists-p knorgpub/index-file)
    (message "Creating index file %s" knorgpub/index-file)
    (knorgpub/create-index-file)))

(defun knorgpub/get-notes-files ()
  (seq-filter (lambda (f)
                (not (string= f (file-name-nondirectory knorgpub/index-file))))
              (directory-files knorgpub/notes-dir nil "\\.org$")))

(defun knorgpub/create-index-file ()
  (let ((title (read-string "Notes title: ")))
    (with-temp-file knorgpub/index-file
      (insert (format "#+title: %s\n#+options: toc:nil\n\n* %s\n\n" title knorgpub/toc-title)))))

(defun knorgpub/extract-headings ()
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (h)
      (let ((title (org-element-property :raw-value h)))
        (unless (string= title org-footnote-section)
          (list :title title
                :level (org-element-property :level h)))))))

(defun knorgpub/extract-note-info (filepath)
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-mode)
    (let* ((filename (file-name-nondirectory filepath))
           (title (or (cadar (org-collect-keywords '("title"))) filename))
           (index (string-to-number (cadar (org-collect-keywords '("index")))))
           (headings (knorgpub/extract-headings)))
      (list :link (concat "./" filename)
            :title title
            :index index
            :headings headings))))

(defun knorgpub/collect-notes ()
  (let ((infos '()))
    (dolist (filename (knorgpub/get-notes-files))
      (message "Processing %s..." filename)
      (push (knorgpub/extract-note-info (expand-file-name filename knorgpub/notes-dir)) infos))
    (sort infos (lambda (a b) (< (plist-get a :index) (plist-get b :index))))))

(defun knorgpub/toc-section (note-info)
  (let* ((link (plist-get note-info :link))
         (title (plist-get note-info :title))
         (index (plist-get note-info :index))
         (headings (plist-get note-info :headings))
         (lines (list (format "** [[%s][%d.0 %s]]" link index title)))
         (counter 0))
    (dolist (h headings)
      (setq counter (1+ counter))
      (let* ((h-title (plist-get h :title))
             (h-level (plist-get h :level))
             (indent (make-string (* (- h-level 1) 2) ?\s))
             (bullet (if (= h-level 1) "+" "-")))
        (push (format "%s%s [[%s::*%s][%d.%d %s]]"
                      indent bullet link h-title index counter h-title)
              lines)))
    (push "" lines)
    (reverse lines)))

(defun knorgpub/update-toc ()
  (with-temp-buffer
    (insert-file-contents knorgpub/index-file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward (format "^\\* %s" knorgpub/toc-title) nil t)
      (beginning-of-line)
      (let ((start (point)))
        (forward-line 1)
        (if (re-search-forward "^\\* " nil t)
            (progn (beginning-of-line) (delete-region start (point)))
          (delete-region start (point-max)))))
    (unless (save-excursion
              (goto-char (point-min))
              (re-search-forward (format "^\\* %s" knorgpub/toc-title) nil t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (insert (format "* %s\n" knorgpub/toc-title))
    (dolist (line (apply #'append (mapcar #'knorgpub/toc-section (knorgpub/collect-notes))))
      (insert line "\n"))
    (write-file knorgpub/index-file)))

;; --- run ---

(knorgpub/validate-env)
(message "Generating TOC in %s..." knorgpub/index-file)
(knorgpub/update-toc)
(message "TOC complete. Processed %d files." (length (knorgpub/get-notes-files)))
