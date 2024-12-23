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

(setq-local k-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head k-html-head)
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
                  :with-author t
                  :with-creator t
                  :with-toc t
                  :with-latex t
                  :section-number nil
                  :time-stamp-file nil)))

(defun run-before-org-export-processing (backend)
  (message "Testing %s" backend)
  (dolist (li (org-babel-src-block-names))
  (if (string-equal "startup" li)
      (progn
        (org-babel-goto-named-src-block li)
        (org-babel-execute-src-block)))))

(setq org-export-before-processing-functions #'run-before-org-export-processing)


(org-publish-all t)

(message "Build complete!")
