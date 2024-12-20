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
(setq org-publish-project-alist
      (list (list "org-site:main"
                  :recursive t
                  :base-directory "./content"
                  :publishing-directory "./public"
                  :publishing-function 'org-html-publish-to-html
                  :with-author t
                  :with-creator t
                  :with-toc t
                  :section-number nil
                  :time-stamp-file nil)))

(org-publish-all t)

(message "Build complete!")
