;;; -*- lexical-binding: t; -*-
;;; Loads config.el from the project root (CWD when invoked).

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))

(use-package htmlize)
(require 'ox-publish)
(require 'ox-html)
(require 'ansi-color)

;; --- configurable defaults (override in config.el) ---
(defvar knorgpub/content-dir    (expand-file-name "content" default-directory))
(defvar knorgpub/public-dir     (expand-file-name "public"  default-directory))
(defvar knorgpub/css-file       "/static/style.css")
(defvar knorgpub/favicon        "/static/favicon.ico")
(defvar knorgpub/scripts        '("toc-toggle.js" "scroll-to-top.js"))
(defvar knorgpub/with-toc       t)
(defvar knorgpub/section-numbers nil)
(defvar knorgpub/babel-header-args
  '((:exports . "both")
    (:results . "output verbatim")
    (:session . "none")
    (:cache   . "no")
    (:noweb   . "no")
    (:hlines  . "no")
    (:tangle  . "no")))

(let ((config (expand-file-name "config.el" default-directory)))
  (unless (file-exists-p config)
    (error "knorgpub: config.el not found in %s - run 'knorgpub init' first" default-directory))
  (load config))

;; --- derived (depend on config) ---
(defvar knorgpub/notes-dir knorgpub/content-dir)

(setq knorgpub/html-scripts
      (mapconcat (lambda (f) (format "<script src=\"/static/%s\"></script>" f))
                 knorgpub/scripts "\n"))

(setq org-html-htmlize-output-type   'inline-css)
(setq org-export-with-sub-superscripts nil)
(setq org-html-doctype               "html5")
(setq org-html-validation-link       nil)
(setq org-html-head-include-scripts  nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head
      (concat
       (format "<link rel=\"stylesheet\" href=\"%s\">\n" knorgpub/css-file)
       (when knorgpub/favicon
         (format "<link rel=\"icon\" href=\"%s\">" knorgpub/favicon))))
(setq org-babel-default-header-args  knorgpub/babel-header-args)

;; --- filters ---

(defun knorgpub/org-ansi-colorize-blocks (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[[:space:]]*:.*" end t)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (match-beginning 0) (match-end 0))))))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (knorgpub/org-ansi-colorize-blocks
               (org-babel-where-is-src-block-result)
               (org-babel-result-end)))))

(defun knorgpub/extract-keyword (filename keyword)
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (when (re-search-forward (format "^[ \t]*#\\+%s:[ \t]*\\(.+\\)[ \t]*$" keyword) nil t)
        (string-trim (match-string 1))))))

(defun knorgpub/breadcrumb-html (crumbs-list)
  (let ((items (mapcar (lambda (crumb)
                         (let ((parts (split-string crumb ":")))
                           (if (= (length parts) 2)
                               (format "<a href=\"%s\">%s</a>"
                                       (string-trim (cadr parts))
                                       (string-trim (car parts)))
                             (format "<span>%s</span>" (string-trim crumb)))))
                       crumbs-list)))
    (format "<nav class=\"breadcrumbs\">\n%s\n</nav>"
            (mapconcat 'identity items " <span class=\"separator\">/</span> "))))

(defun knorgpub/nav-buttons-html (buttons-list)
  (let ((items (mapcar (lambda (button)
                         (let ((parts (split-string button ":")))
                           (if (= (length parts) 2)
                               (format "<a href=\"%s\" class=\"nav-button\">%s</a>"
                                       (string-trim (cadr parts))
                                       (string-trim (car parts)))
                             (format "<span class=\"nav-button disabled\">%s</span>"
                                     (string-trim button)))))
                       buttons-list)))
    (format "<div class=\"nav-buttons\">\n%s\n</div>"
            (mapconcat 'identity items "\n"))))

(defun knorgpub/filter-nav-buttons (string backend info)
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (back (knorgpub/extract-keyword input-file "back"))
           (next (knorgpub/extract-keyword input-file "next")))
      (when (or back next)
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (when (re-search-forward "<h1[^>]*class=\"title\"[^>]*>.*?</h1>" nil t)
            (insert "\n" (knorgpub/nav-buttons-html (delq nil (list back next)))))
          (buffer-string))))))

(defun knorgpub/filter-breadcrumbs (string backend info)
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (breadcrumbs (knorgpub/extract-keyword input-file "breadcrumbs")))
      (when breadcrumbs
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (when (re-search-forward "<h1[^>]*class=\"title\"[^>]*>.*?</h1>" nil t)
            (insert "\n" (knorgpub/breadcrumb-html
                          (append (split-string breadcrumbs " > ") '(".")))))
          (buffer-string))))))

(defun knorgpub/filter-collapsible-toc (string backend _info)
  (when (eq backend 'html)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (when (re-search-forward
             "<div id=\"table-of-contents\" role=\"doc-toc\">\n<h2>\\([^<]+\\)</h2>" nil t)
        (replace-match
         "<div id=\"table-of-contents\" role=\"doc-toc\">\n<div class=\"toc-header\" onclick=\"toggleToc()\">\n<h2>\\1</h2>\n<div class=\"toggle-icon\">+</div>\n</div>"))
      (buffer-string))))

(defun knorgpub/filter-footer (string backend info)
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (breadcrumbs (knorgpub/extract-keyword input-file "breadcrumbs"))
           (back (knorgpub/extract-keyword input-file "back"))
           (next (knorgpub/extract-keyword input-file "next")))
      (if (or breadcrumbs back next)
          (with-temp-buffer
            (insert string)
            (goto-char (point-max))
            (search-backward "</body>" nil t)
            (let* ((crumbs-html
                    (when breadcrumbs
                      (replace-regexp-in-string
                       "class=\"breadcrumbs\""
                       "class=\"breadcrumbs footer\""
                       (knorgpub/breadcrumb-html
                        (append (split-string breadcrumbs " > ") '("."))))))
                   (nav-html (knorgpub/nav-buttons-html (delq nil (list back next)))))
              (insert "\n<div class=\"footer\">\n"
                      "<button class=\"scroll-to-top\" onclick=\"scrollToTop()\">^ home</button>\n\n"
                      (or crumbs-html "")
                      "\n"
                      nav-html
                      "\n</div>\n"))
            (buffer-string))
        string))))

(add-to-list 'org-export-filter-final-output-functions 'knorgpub/filter-collapsible-toc)
(add-to-list 'org-export-filter-final-output-functions 'knorgpub/filter-breadcrumbs)
(add-to-list 'org-export-filter-final-output-functions 'knorgpub/filter-nav-buttons)
(add-to-list 'org-export-filter-final-output-functions 'knorgpub/filter-footer)

;; --- publish ---

(setq org-publish-project-alist
      `(("knorgpub-content"
         :base-directory ,knorgpub/notes-dir
         :publishing-directory ,knorgpub/public-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-creator nil
         :with-toc ,knorgpub/with-toc
         :section-numbers ,knorgpub/section-numbers
         :time-stamp-file nil
         :html-head-extra ,knorgpub/html-scripts)
        ("knorgpub" :components ("knorgpub-content"))))

(org-publish-all t)

;; remove orphaned html files
(let* ((output-dir (expand-file-name knorgpub/public-dir))
       (valid-html
        (mapcar (lambda (f)
                  (concat (file-name-sans-extension
                           (expand-file-name (file-relative-name f knorgpub/notes-dir) output-dir))
                          ".html"))
                (directory-files-recursively knorgpub/notes-dir "\\.org$"))))
  (dolist (html-file (directory-files-recursively output-dir "\\.html$"))
    (unless (member html-file valid-html)
      (delete-file html-file)
      (message "Deleted orphan: %s" html-file))))

(message "Build complete!")
