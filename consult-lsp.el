;;; consult-lsp.el --- LSP Consult integration -*- lexical-binding: t; -*-

;; Licence: MIT
;; Keywords: completion, lsp
;; Author: Gerry Agbobada
;; Maintainer: Gerry Agbobada
;; Package-Requires: ((emacs "26.1") (lsp-mode "5.0") (consult "0.6"))
;; Version: 0.1
;; Homepage: https://github.com/gagbo/consult-lsp

;;; Commentary:
;; Provides LSP-mode related commands for consult

;;; Code:

(require 'consult)
(require 'lsp)

(defun +consult--lsp-flatten-diagnostics (transformer &optional current-workspace?)
  "Flatten the list of LSP-mode diagnostics to consult candidates.

TRANSFORMER takes (file diag) and returns a suitable element for
`completing-read'.
CURRENT-WORKSPACE? has the same meaning as in `lsp-diagnostics'."
  (append
   (ht-map
    (lambda (file diags)
      (mapcar (lambda (diag) (funcall transformer file diag))
              diags))
    (lsp-diagnostics current-workspace?))))

(defun severity-to-level (sev)
  "Convert diagnostic severity SEV to a string."
  (cond ((= sev 3) "error")
        ((= sev 2) "warning")
        ((= sev 1) "info")
        (t "unknown")))

(defun test-transformer (file diag)
  "Transform LSP-mode diagnostics from a pair FILE DIAG to a candidate."
  (propertize
   (format "%-7s %-80.80s %-4d %-10.10s %.30S"
           (severity-to-level (ht-get diag "severity" 0))
           file
           (ht-get (ht-get (ht-get diag "range") "start") "line" 0)
           (ht-get diag "source" "unknown")
           (ht-get diag "message"))
   ;; TODO: 'consult--candidate -> marker at the beginning of diag
   ;; It should use (ht-get (ht-get diag "range") "start") ?
   'consult--type (ht-get diag "severity" "unknown")))
;; message
;; source
;; code
;; severity
;; range
;; tags
;; relatedInformation

;;;###autoload
(defun +consult/lsp-diagnostics ()
  "Query LSP-mode diagnostics."
  (interactive)
  (consult--read (+consult--lsp-flatten-diagnostics 'test-transformer)
                 :prompt "LSP Diagnostics "
                 :require-match t
                 :history t
                 :category 'consult-lsp-diagnostics
                 :sort nil
                 :lookup #'consult--lookup-candidate))

(provide 'consult-lsp)

;;; consult-lsp.el ends here
