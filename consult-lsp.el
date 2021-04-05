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


;;;; Diagnostics

(defun consult-lsp-diagnostics--flatten-diagnostics (transformer &optional current-workspace?)
  "Flatten the list of LSP-mode diagnostics to consult candidates.

TRANSFORMER takes (file diag) and returns a suitable element for
`consult--read'.
CURRENT-WORKSPACE? has the same meaning as in `lsp-diagnostics'."
  (sort
   (flatten-list
    (ht-map
     (lambda (file diags)
       (mapcar (lambda (diag) (funcall transformer file diag))
               diags))
     (lsp-diagnostics current-workspace?)))
   ;; Sort by ascending severity
   (lambda (cand-left cand-right)
     (let* ((diag-left (get-text-property 0 'consult--candidate cand-left))
            (diag-right (get-text-property 0 'consult--candidate cand-right))
            (sev-left (or (lsp:diagnostic-severity? diag-left) 12))
            (sev-right (or (lsp:diagnostic-severity? diag-right) 12)))
       (< sev-left sev-right)))))

(defun consult-lsp-diagnostics--severity-to-level (diag)
  "Convert diagnostic severity of DIAG to a string."
  (let ((sev (lsp:diagnostic-severity? diag)))
    (cond ((= sev 1) (propertize "error" 'face 'error))
          ((= sev 2) (propertize "warn" 'face 'warning))
          ((= sev 3) (propertize "info" 'face 'success))
          ((= sev 4) (propertize "hint" 'face 'italic))
          (t "unknown"))))

(defun consult-lsp-diagnostics--severity-to-type (diag)
  "Convert diagnostic severity of DIAG to a type for consult--type."
  (let ((sev (lsp:diagnostic-severity? diag)))
    (cond ((= sev 1) ?e)
          ((= sev 2) ?w)
          ((= sev 3) ?i)
          ((= sev 4) ?h)
          (t ?u))))

(defconst consult-lsp-diagnostics--narrow
  '((?e . "Errors")
    (?w . "Warnings")
    (?i . "Infos")
    (?h . "Hints")
    (?u . "Unknown"))
  "Set the narrow keys for consult-lsp")

(defun consult-lsp-diagnostics--source (diag)
  "Convert source of DIAG to a propertized string."
  (propertize (lsp:diagnostic-source? diag) 'face 'success))

(defun consult-lsp-diagnostics--diagnostic-marker (file diag)
  "Return a marker at DIAG beginning."
  (consult--position-marker
   file
   (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range diag)))))
   (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range diag)))))))

(defun test-transformer (file diag)
  "Transform LSP-mode diagnostics from a pair FILE DIAG to a candidate."
  (propertize
   (format "%-4s %-60.60s %.15s %s"
           (consult-lsp-diagnostics--severity-to-level diag)
           (consult--format-location
            file
            (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line))))
           (consult-lsp-diagnostics--source diag)
           (string-replace "\n" " " (lsp:diagnostic-message diag)))
   'consult--candidate (cons file diag)
   'consult--type (consult-lsp-diagnostics--severity-to-type diag)))

(defun consult-lsp-diagnostics--state ()
  "LSP diagnostic preview."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open nil))
      (message "Calling preview on %S" cand)
      (funcall jump
               (consult--position-marker
                (and (car cand) (funcall (if restore #'find-file open) (car cand)))
                (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range (cdr cand))))))
                (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range (cdr cand)))))))
               restore))))

;;;###autoload
(defun consult-lsp-diagnostics (arg)
  "Query LSP-mode diagnostics. When ARG is set through prefix, query all workspaces."
  (interactive "P")
  (let ((all-workspaces? arg))
    (consult--read (consult-lsp-diagnostics--flatten-diagnostics 'test-transformer (not all-workspaces?))
                   :prompt (concat  "LSP Diagnostics " (when arg "(all workspaces) "))
                   :require-match t
                   :history t
                   :category 'consult-lsp-diagnostics
                   :sort nil
                   :predicate
                   (lambda (cand)
                     (let ((key (get-text-property 0 'consult--type cand)))
                       (if consult--narrow
                           (= key consult--narrow)
                         t)))
                   :title (consult--type-title consult-lsp-diagnostics--narrow)
                   :narrow (consult--type-narrow consult-lsp-diagnostics--narrow)
                   :state (consult-lsp-diagnostics--state)
                   :lookup #'consult--lookup-candidate)))


;;;; Symbols

;; TODO: write the consult-lsp-symbols--narrow boilerplate
;;      (the 26 symbol-kinds)
;; TODO: extract the title function in a lambda
;; TODO: use the narrow boilerplate in the title function
;; TODO: extract the candidate building lambda in a "transformer"
;; TODO: Make the function not try to jump if the user exits with ESC
;; TODO: Make the command properly use only the current workspace without prefix arg


(defun consult-lsp-symbols--state ()
  "LSP diagnostic preview."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open nil))
      (funcall jump
               (let* ((location (lsp:symbol-information-location cand))
                      (uri (lsp:location-uri location)))
                 (consult--position-marker
                  (and uri (funcall (if restore #'find-file open) (lsp--uri-to-path uri)))
                  (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:location-range location)))))
                  (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:location-range location)))))))
               restore))))

;; "Create ASYNC sink function.
;; The async function should accept a single action argument.
;; Only for the 'setup action, it is guaranteed that the call
;; originates from the minibuffer. For the other actions no
;; assumptions can be made.
;; Depending on the argument, the caller context differ.
;; 'setup   Setup the internal state.
;; 'destroy Destroy the internal state.
;; 'flush   Flush the list of candidates.
;; 'refresh Request UI refresh.
;; nil      Get the list of candidates.
;; List     Append the list to the list of candidates.
;; String   The input string, called when the user enters something."

;; It is an async source because some servers, like rust-analyzer, send a
;; max count of results for queries (120 last time checked). Therefore, in
;; big projects the first query might not have the target result to filter on.
;; To avoid this issue, we use an async source that retriggers the request.
(defun consult-lsp-symbols--make-async-source (async workspaces)
  "Make a consult--read compatible async-source for symbols in WORKSPACES."
  (let ((request-id)
        (cancel-token :consult-lsp-symbols))
    (lambda (action)
      (pcase-exhaustive action
        ((pred stringp)
         (lsp-cancel-request-by-token cancel-token)
         (with-lsp-workspaces workspaces
           (-let (((request &as &plist :id new-request-id)))
             (setq request-id new-request-id)
             (consult--async-log "consult-lsp-symbols request started for %S\n" action)
             (lsp-request-async
              "workspace/symbol"
              (list :query action)
              (lambda (res)
                ;; Flush old candidates list
                (funcall async 'flush)
                (funcall async
                         (mapcar
                          ;; TODO: extract this transformer and
                          ;; use consult--async-map instead
                          (lambda (symbol-info)
                            (propertize
                             (format "%-7s %-5s %s %s"
                                     (alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds)
                                     (lsp:symbol-information-container-name? symbol-info)
                                     (lsp:symbol-information-name symbol-info)
                                     (rng-uri-file-name (lsp:location-uri (lsp:symbol-information-location symbol-info))))
                             'consult--candidate symbol-info))
                          res)))
              :mode 'detached
              :no-merge nil
              :cancel-token cancel-token)))
         (funcall async action))
        ('destroy
         (lsp-cancel-request-by-token cancel-token)
         (funcall async action))
        (_ (funcall async action))))))

(defun consult-lsp-symbols ()
  "Query workspace symbols."
  (interactive)
  (let ((ws (or (lsp-workspaces)
                (gethash (lsp-workspace-root default-directory)
                         (lsp-session-folder->servers (lsp-session)))
                (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session))))))))
    (consult--read
     (thread-first
         (consult--async-sink)
       (consult--async-refresh-timer)
       (consult-lsp-symbols--make-async-source ws)
       (consult--async-throttle))
     :prompt "LSP Symbols "
     :require-match t
     :history t
     :category 'consult-lsp-diagnostics
     :lookup #'consult--lookup-candidate
     :title (lambda (cand)
              (alist-get
               (lsp:symbol-information-kind
                (get-text-property 0 'consult--candidate cand))
               lsp-symbol-kinds))
     :state (consult-lsp-symbols--state))))


(provide 'consult-lsp)
;;; consult-lsp.el ends here
