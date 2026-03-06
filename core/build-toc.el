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

(defun knorgpub/create-index-file ()
  (let ((title (read-string "Notes title: ")))
    (with-temp-file knorgpub/index-file
      (insert (format "#+title: %s\n#+options: toc:nil\n\n* %s\n\n" title knorgpub/toc-title)))))

(defun knorgpub/ensure-subdir-index (subdir)
  "Ensure index.org exists in SUBDIR, auto-creating with default content if missing.
Returns the path to the index file."
  (let ((index (expand-file-name "index.org" subdir)))
    (unless (file-exists-p index)
      (let ((title (capitalize (file-name-nondirectory subdir))))
        (message "Creating index for subdirectory: %s" subdir)
        (with-temp-file index
          (insert (format "#+title: %s\n#+index: 0\n#+options: toc:nil\n\n* %s\n\n"
                          title knorgpub/toc-title)))))
    index))

(defun knorgpub/get-dir-org-files (dir index-file)
  "Return absolute paths of .org files directly in DIR, excluding INDEX-FILE."
  (seq-filter (lambda (f) (not (string= f index-file)))
              (directory-files dir t "\\.org$")))

(defun knorgpub/get-subdirs (dir)
  "Return absolute paths of subdirectories in DIR, excluding hidden ones."
  (seq-filter (lambda (f)
                (and (file-directory-p f)
                     (not (string-match-p "/\\." f))))
              (directory-files dir t)))

(defun knorgpub/extract-headings ()
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (h)
      (let* ((raw   (org-element-property :raw-value h))
             (title (string-trim (replace-regexp-in-string "\\[fn:[^]]+\\]" "" raw))))
        (unless (string= title org-footnote-section)
          (list :title title
                :level (org-element-property :level h)
                :linkable (string= raw title)))))))

(defun knorgpub/extract-file-info (filepath dir)
  "Extract TOC entry info from FILEPATH. DIR is the directory of the index being built."
  (with-temp-buffer
    (insert-file-contents filepath)
    (org-mode)
    (let* ((rel-path (file-relative-name filepath dir))
           (title    (or (cadar (org-collect-keywords '("title"))) (file-name-base filepath)))
           (index    (string-to-number (or (cadar (org-collect-keywords '("index"))) "0")))
           (headings (knorgpub/extract-headings)))
      (list :link     (concat "./" rel-path)
            :title    title
            :index    index
            :headings headings
            :type     'file))))

(defun knorgpub/extract-subdir-info (subdir-index dir)
  "Extract TOC entry info for a subdirectory from its SUBDIR-INDEX file.
DIR is the directory of the parent index being built."
  (with-temp-buffer
    (insert-file-contents subdir-index)
    (org-mode)
    (let* ((rel-path (file-relative-name subdir-index dir))
           (title    (or (cadar (org-collect-keywords '("title")))
                         (capitalize (file-name-nondirectory
                                      (directory-file-name (file-name-directory subdir-index))))))
           (index    (string-to-number (or (cadar (org-collect-keywords '("index"))) "0"))))
      (list :link     (concat "./" rel-path)
            :title    title
            :index    index
            :headings nil
            :type     'subdir))))

(defun knorgpub/toc-entry (item)
  "Generate list of TOC lines for ITEM."
  (let* ((link     (plist-get item :link))
         (title    (plist-get item :title))
         (index    (plist-get item :index))
         (headings (plist-get item :headings))
         (type     (plist-get item :type))
         (lines    (list (if (eq type 'subdir)
                            (format "** [[%s][%d %s]]" link index title)
                          (format "** [[%s][%d.0 %s]]" link index title))))
         (counter  0))
    (when (eq type 'file)
      (dolist (h headings)
        (setq counter (1+ counter))
        (let* ((h-title    (plist-get h :title))
               (h-level    (plist-get h :level))
               (h-linkable (plist-get h :linkable))
               (indent     (make-string (* (- h-level 1) 2) ?\s))
               (bullet     (if (= h-level 1) "+" "-"))
               (target     (if h-linkable (format "%s::*%s" link h-title) link)))
          (push (format "%s%s [[%s][%d.%d %s]]"
                        indent bullet target index counter h-title)
                lines))))
    (push "" lines)
    (reverse lines)))

(defun knorgpub/update-index (index-file items)
  "Replace the TOC section in INDEX-FILE with entries generated from ITEMS."
  (with-temp-buffer
    (insert-file-contents index-file)
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
    (dolist (line (apply #'append (mapcar #'knorgpub/toc-entry items)))
      (insert line "\n"))
    (write-file index-file)))

(defun knorgpub/process-directory (dir)
  "Recursively process DIR: ensure index.org exists, update its TOC, process subdirs."
  (let* ((index-file (expand-file-name "index.org" dir))
         (org-files  (knorgpub/get-dir-org-files dir index-file))
         (subdirs    (knorgpub/get-subdirs dir))
         (items      '()))
    ;; Recursively process subdirs and collect their index entries
    (dolist (subdir subdirs)
      (let ((subdir-index (knorgpub/ensure-subdir-index subdir)))
        (knorgpub/process-directory subdir)
        (push (knorgpub/extract-subdir-info subdir-index dir) items)))
    ;; Collect .org file entries
    (dolist (filepath org-files)
      (message "Processing %s..." filepath)
      (push (knorgpub/extract-file-info filepath dir) items))
    ;; Sort by #+index: value
    (setq items (sort items (lambda (a b) (< (plist-get a :index) (plist-get b :index)))))
    ;; Write updated index
    (knorgpub/update-index index-file items)))

;; --- run ---

(knorgpub/validate-env)
(message "Generating TOCs starting from %s..." knorgpub/notes-dir)
(knorgpub/process-directory knorgpub/notes-dir)
(message "TOC generation complete.")
