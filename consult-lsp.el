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

(defun consult-lsp--flatten-diagnostics (transformer &optional current-workspace?)
  "Flatten the list of LSP-mode diagnostics to consult candidates.

TRANSFORMER takes (file diag) and returns a suitable element for
`completing-read'.
CURRENT-WORKSPACE? has the same meaning as in `lsp-diagnostics'."
  (sort
   (append
    (ht-map
     (lambda (file diags)
       (mapcar (lambda (diag) (funcall transformer file diag))
               diags))
     (lsp-diagnostics current-workspace?)))
   ;; Sort by ascending severity
   (lambda (cand-left cand-right)
     (let* ((diag-left (get-text-property 0 'consult--candidate (car cand-left)))
            (diag-right (get-text-property 0 'consult--candidate (car cand-right)))
            (sev-left (or (lsp:diagnostic-severity? diag-left) 12))
            (sev-right (or (lsp:diagnostic-severity? diag-right) 12)))
       (< sev-left sev-right)))))

(defun consult-lsp--severity-to-level (diag)
  "Convert diagnostic severity of DIAG to a string."
  (let ((sev (lsp:diagnostic-severity? diag)))
    (cond ((= sev 1) (propertize "error" 'face 'error))
          ((= sev 2) (propertize "warn" 'face 'warning))
          ((= sev 3) (propertize "info" 'face 'success))
          ((= sev 4) (propertize "hint" 'face 'italic))
          (t "unknown"))))

(defun consult-lsp--severity-to-type (diag)
  "Convert diagnostic severity of DIAG to a type for consult--type."
  (let ((sev (lsp:diagnostic-severity? diag)))
    (cond ((= sev 1) ?e)
          ((= sev 2) ?w)
          ((= sev 3) ?i)
          ((= sev 4) ?h)
          (t ?u))))

(defconst consult-lsp--narrow
  '((?e . "Errors")
    (?w . "Warnings")
    (?i . "Infos")
    (?h . "Hints")
    (?u . "Unknown"))
  "Set the narrow keys for consult-lsp")

(defun consult-lsp--source (diag)
  "Convert source of DIAG to a propertized string."
  (propertize (lsp:diagnostic-source? diag) 'face 'success))

(defun consult-lsp--diagnostic-marker (file diag)
  "Return a marker at DIAG beginning."
  (consult--position-marker
   file
   (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range diag)))))
   (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range diag)))))))

(defun test-transformer (file diag)
  "Transform LSP-mode diagnostics from a pair FILE DIAG to a candidate."
  (propertize
   (format "%-4s %-60.60s %.15s %s"
           (consult-lsp--severity-to-level diag)
           (consult--format-location
            file
            (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line))))
           (consult-lsp--source diag)
           (string-replace "\n" " " (lsp:diagnostic-message diag)))
   ;; TODO: Will that consult-lsp--diagnostic-marker eagerly open all files ?
   'consult-location (consult-lsp--diagnostic-marker file diag)
   'consult-location-list (list file diag)
   'consult--candidate diag
   'consult--type (consult-lsp--severity-to-type diag)))
;; message
;; source
;; code
;; severity
;; range
;; tags
;; relatedInformation

(defun consult-lsp-diagnostics--preview (display)
  "Xref preview with DISPLAY function."
  (let ((open (consult--temporary-files))
        (preview (consult--jump-preview)))
    (lambda (cand restore)
      (cond
       (restore
        (funcall preview nil t)
        (funcall open nil))
       (cand
        (let ((loc-list (get-text-property 0 'consult-location-list (car cand)))
              (consult--buffer-display display))
          (message "Calling preview on %S" loc-list)
          (funcall preview
                   (consult--position-marker
                    (funcall open (car loc-list))
                    (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range (cdr loc-list))))))
                    (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range (cdr loc-list)))))))
                   nil)))))))

;;;###autoload
(defun consult-lsp-diagnostics ()
  "Query LSP-mode diagnostics."
  (interactive)
  (consult--read (consult-lsp--flatten-diagnostics 'test-transformer)
                 :prompt "LSP Diagnostics "
                 :require-match t
                 :history t
                 :category 'consult-lsp-diagnostics
                 :sort nil
                 :title (consult--type-title consult-lsp--narrow)
                 :narrow (consult--type-narrow consult-lsp--narrow)
                 :state (consult-lsp-diagnostics--preview #'switch-to-buffer-other-window)
                 :lookup #'consult--lookup-location))

(provide 'consult-lsp)
;;; consult-lsp.el ends here
