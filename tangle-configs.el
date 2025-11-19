;;; tangle-configs.el --- Tangle all Emacs configuration files  -*- lexical-binding: t; -*-

;;; Commentary:
;; This script tangles all org configuration files in the configs/ directory
;; and its subdirectories (like programming/)

;;; Code:

(require 'org)

(defun tangle-org-files-recursively (directory)
  "Find and tangle all .org files in DIRECTORY and its subdirectories."
  (let ((tangled-count 0))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      ;; Skip backup files
      (unless (string-match-p "\\.org\\.backup$" file)
        (message "Tangling: %s" (file-relative-name file directory))
        (org-babel-tangle-file file)
        (setq tangled-count (1+ tangled-count))))
    tangled-count))

(defun tangle-config-files ()
  "Tangle all .org files in the configs/ directory and subdirectories."
  (let ((config-dir (expand-file-name "content/emacs/configs" default-directory)))
    (unless (file-directory-p config-dir)
      (error "Config directory not found: %s" config-dir))

    (message "Tangling configuration files from: %s" config-dir)
    (message "Including subdirectories (e.g., programming/)")

    (let ((tangled-files (tangle-org-files-recursively config-dir)))
      (message "Successfully tangled %d configuration files!" tangled-files))))

;; Run the function
(tangle-config-files)

(provide 'tangle-configs)
;;; tangle-configs.el ends here
