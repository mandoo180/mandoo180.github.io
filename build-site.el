;;; build-site.el --- Summary:  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; prepare for dependencies
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install 'htmlize)

;; requires org export publish
(require 'ox-publish)
(require 'ox-html)

(setq local/org-html-head (concat "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                                  "<link rel=\"stylesheet\" href=\"/assets/styles/style.css\">"
                                  "<link rel=\"stylesheet\" href=\"/assets/styles/toc-sidebar.css\">"
                                  "<script src=\"/assets/scripts/toc-sidebar.js\" defer></script>"))
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
                  :recursive            t
                  :base-extension       "org"
                  :base-directory       "./content"
                  :publishing-directory "./public"
                  :publishing-function  'org-html-publish-to-html
                  :with-author          nil
                  :with-creator         nil
                  :with-toc             t
                  :with-latex           t
                  :with-todo-keywords   nil
                  :section-number       nil
                  :time-stamp-file      nil)))

;; No post-export filter needed - styling is handled by CSS

(let ((content-dir (directory-file-name "content"))
	  (public-dir  (directory-file-name "public")))
  (delete-directory (expand-file-name public-dir) t)
  (copy-directory (expand-file-name "assets" content-dir) (expand-file-name "assets" public-dir) t t t)
  (org-publish-all t))

(message "Build complete!")
;;; build-site.el ends here
