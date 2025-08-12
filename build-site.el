;;; build-site.el --- Summary:  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; prepare for dependencies
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages.")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install 'htmlize)

;; requires org export publish
(require 'ox-publish)

(setq-local local/script-tag "<script>window.tailwind = window.tailwind || {};window.tailwind.config = { darkMode: 'media' }; /* follow browser */</script>")
(setq-local local/style-tag "
<style>
  :root { color-scheme: light dark; }      /* hint to UA widgets, form controls */
  body { margin:0; }
  @media (prefers-color-scheme: light) { body { background: #ffffff; } }
  @media (prefers-color-scheme: dark)  { body { background: #0b0b0b; } }
</style>")
(setq-local local/org-html-head (concat "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
										"<script src=\"https://cdn.tailwindcss.com?plugins=typography\"></script>"
										local/script-tag
										local/style-tag))
(setq org-html-validation-link            nil
	  org-html-htmlize-output-type        'css
      org-html-head-include-scripts       nil
      org-html-head-include-default-style nil
      org-html-head                       local/org-html-head
	  org-html-divs                       '((preamble "header" "preamble")
											(content "main" "content")
											(postamble "footer" "postamble")))
(setq org-hide-emphasis-markers t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-property-inheritance t)
(setq org-babel-min-lines-for-block-output 0)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (emacs-lisp . t)))
(setq org-publish-project-alist
      (list (list "org-site:main"
                  :recursive t
                  :base-extension "org"
                  :base-directory "./content"
                  :publishing-directory "./public"
                  :publishing-function 'org-html-publish-to-html
                  :with-author nil
                  :with-creator t
                  :with-toc t
                  :with-latex t
                  :with-todo-keywords nil
                  :section-number nil
                  :time-stamp-file nil)))

(defun local/org-html-add-tailwind-container (output backend info)
  "After-export filter to add Tailwind classes to the content wrapper."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "<main id=\"content\" class=\"content\">"
     "<main id=\"content\" class=\"prose prose-zinc dark:prose-invert mx-auto w-full max-w-3xl md:max-w-4xl lg:max-w-5xl xl:max-w-6xl px-4 sm:px-6 lg:px-8\">"
     output t t)))

(add-to-list 'org-export-filter-final-output-functions #'local/org-html-add-tailwind-container)

;; (defun run-before-org-export-processing (backend)
;;   (message "Testing %s" backend)
;;   (dolist (li (org-babel-src-block-names))
;;   (if (string-equal "startup" li)
;;       (progn
;;         (org-babel-goto-named-src-block li)
;;         (org-babel-execute-src-block)))))

;; for testing purpose
;; (setq org-export-before-processing-functions #'run-before-org-export-processing)

(let ((content-dir (directory-file-name "content"))
	  (public-dir (directory-file-name "public")))
  (delete-directory (expand-file-name public-dir) t)
  (copy-directory (expand-file-name "assets" content-dir) (expand-file-name "assets" public-dir) t t t)
  (org-publish-all t))

(message "Build complete!")
;;; build-site.el ends here
