;;; consult-lsp.el --- LSP-mode Consult integration -*- lexical-binding: t; -*-

;; Licence: MIT
;; Keywords: tools, completion, lsp
;; Author: Gerry Agbobada
;; Maintainer: Gerry Agbobada
;; Package-Requires: ((emacs "27.1") (lsp-mode "5.0") (consult "0.6"))
;; Version: 0.1
;; Homepage: https://github.com/gagbo/consult-lsp

;; Copyright (c) 2021 Gerry Agbobada
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Provides LSP-mode related commands for consult

;; TODO: Use a custom format for the propertized candidates
;; TODO: Format the filenames better
;; TODO: Check the properties in LSP to see how sources should be used
;; TODO: Fix weird behaviour in consult-lsp-symbols
;;       (probably needs a "split" filter in the async chain)
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
  "Set of narrow keys for `consult-lsp-diagnostics'.")

(defun consult-lsp-diagnostics--source (diag)
  "Convert source of DIAG to a propertized string."
  (propertize (lsp:diagnostic-source? diag) 'face 'success))

(defun consult-lsp-diagnostics--diagnostic-marker (file diag)
  "Return a marker in FILE at the beginning of DIAG."
  (consult--position-marker
   file
   (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range diag)))))
   (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range diag)))))))

(defun consult-lsp-diagnostics--transformer (file diag)
  "Transform LSP-mode diagnostics from a pair FILE DIAG to a candidate."
  (propertize
   (format "%-4s %-60.60s %.15s %s"
           (consult-lsp-diagnostics--severity-to-level diag)
           (consult--format-location
            file
            (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line))))
           (consult-lsp-diagnostics--source diag)
           (replace-regexp-in-string "\n" " " (lsp:diagnostic-message diag)))
   'consult--candidate (cons file diag)
   'consult--type (consult-lsp-diagnostics--severity-to-type diag)))

(defun consult-lsp-diagnostics--state ()
  "LSP diagnostic preview."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open nil))
      (when cand
        (funcall jump
                 (consult--position-marker
                  (and (car cand) (funcall (if restore #'find-file open) (car cand)))
                  (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range (cdr cand))))))
                  (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range (cdr cand)))))))
                 restore)))))

;;;###autoload
(defun consult-lsp-diagnostics (arg)
  "Query LSP-mode diagnostics. When ARG is set through prefix, query all workspaces."
  (interactive "P")
  (let ((all-workspaces? arg))
    (consult--read (consult-lsp-diagnostics--flatten-diagnostics #'consult-lsp-diagnostics--transformer (not all-workspaces?))
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

(defconst consult-lsp-symbols--narrow
  '(
    ;; Lowercase classes
    (?c . "Class")
    (?f . "Field")
    (?e . "Enum")
    (?i . "Interface")
    (?m . "Module")
    (?n . "Namespace")
    (?p . "Package")
    (?s . "Struct")
    (?t . "Type Parameter")
    (?v . "Variable")

    ;; Uppercase classes
    (?A . "Array")
    (?B . "Boolean")
    (?C . "Constant")
    (?E . "Enum Member")
    (?F . "Function")
    (?M . "Method")
    (?N . "Number")
    (?O . "Object")
    (?P . "Property")
    (?S . "String")

    (?o . "Other")
    ;; Types included in "Other" (i.e. the ignored)
    ;; (?n . "Null")
    ;; (?c . "Constructor")
    ;; (?e . "Event")
    ;; (?k . "Key")
    ;; (?o . "Operator")
    )
  "Set of narrow keys for `consult-lsp-symbols'.")

(defun consult-lsp-symbols--kind-to-narrow (symbol-info)
  "Get the narrow character for SYMBOL-INFO."
  (pcase-exhaustive (alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds)
    ("Class" ?c)
    ("Field" ?f)
    ("Enum" ?e)
    ("Interface" ?i)
    ("Module" ?m)
    ("Namespace" ?n)
    ("Package" ?p)
    ("Struct" ?s)
    ("Type Parameter" ?t)
    ("Variable" ?v)
    ("Array" ?A)
    ("Boolean" ?B)
    ("Constant" ?C)
    ("Enum Member" ?E)
    ("Function" ?F)
    ("Method" ?M)
    ("Number" ?N)
    ("Object" ?O)
    ("Property" ?P)
    ("String" ?S)
    (_ ?o)))


(defun consult-lsp-symbols--state ()
  "Return a LSP symbol preview function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open nil))
      (when cand
        (funcall jump
                 (let* ((location (lsp:symbol-information-location cand))
                        (uri (lsp:location-uri location)))
                   (consult--position-marker
                    (and uri (funcall (if restore #'find-file open) (lsp--uri-to-path uri)))
                    (thread-first location
                      (lsp:location-range)
                      (lsp:range-start)
                      (lsp:position-line)
                      (1+)
                      (lsp-translate-line))
                    (thread-first location
                      (lsp:location-range)
                      (lsp:range-start)
                      (lsp:position-character)
                      (1+)
                      (lsp-translate-column))))
                 restore)))))

;; It is an async source because some servers, like rust-analyzer, send a
;; max count of results for queries (120 last time checked). Therefore, in
;; big projects the first query might not have the target result to filter on.
;; To avoid this issue, we use an async source that retriggers the request.
(defun consult-lsp-symbols--make-async-source (async workspaces)
  "Pipe a consult--read compatible async-source ASYNC to search for symbols in WORKSPACES."
  (let ((cancel-token :consult-lsp-symbols))
    (lambda (action)
      (pcase-exhaustive action
        ((or 'setup (pred stringp))
         (let ((query (if (stringp action) action "")))
           (with-lsp-workspaces workspaces
             (-let (((request &as &plist :id new-request-id)))
               (setq request-id new-request-id)
               (consult--async-log "consult-lsp-symbols request started for %S\n" action)
               (lsp-request-async
                "workspace/symbol"
                (list :query query)
                (lambda (res)
                  ;; Flush old candidates list
                  (funcall async 'flush)
                  (funcall async res))
                :mode 'detached
                :no-merge nil
                :cancel-token cancel-token))))
         (funcall async action))
        ('destroy
         (lsp-cancel-request-by-token cancel-token)
         (funcall async action))
        (_ (funcall async action))))))

(defun consult-lsp-symbols--transformer (symbol-info)
  "Default transformer to produce a completion candidate from SYMBOL-INFO."
  (propertize
   (format "%-7s %-5s %s %s"
           (alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds)
           (lsp:symbol-information-container-name? symbol-info)
           (lsp:symbol-information-name symbol-info)
           (consult--format-location
            (rng-uri-file-name (lsp:location-uri (lsp:symbol-information-location symbol-info)))
            (thread-first symbol-info
              (lsp:symbol-information-location)
              (lsp:location-range)
              (lsp:range-start)
              (lsp:position-line)
              (1+)
              (lsp-translate-line))))
   'consult--type (consult-lsp-symbols--kind-to-narrow symbol-info)
   'consult--candidate symbol-info))

;;;###autoload
(defun consult-lsp-symbols (arg)
  "Query workspace symbols. When ARG is set through prefix, query all workspaces."
  (interactive "P")
  (let* ((all-workspaces? arg)
         (ws (or (and all-workspaces? (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session))))))
                 (lsp-workspaces)
                 (gethash (lsp-workspace-root default-directory)
                          (lsp-session-folder->servers (lsp-session))))))
    (unless ws
      (user-error "There is no active workspace !"))
    (consult--read
     (thread-first
         (consult--async-sink)
       (consult--async-refresh-immediate)
       (consult--async-map #'consult-lsp-symbols--transformer)
       (consult-lsp-symbols--make-async-source ws)
       (consult--async-throttle)
       (consult--async-split))
     :prompt "LSP Symbols "
     :require-match t
     :history t
     :initial consult-async-default-split
     :category 'consult-lsp-symbols
     :lookup #'consult--lookup-candidate
     :predicate
     (lambda (cand)
       (let ((key (get-text-property 0 'consult--type cand)))
         (if consult--narrow
             (= key consult--narrow)
           t)))
     :title (consult--type-title consult-lsp-symbols--narrow)
     :narrow (consult--type-narrow consult-lsp-symbols--narrow)
     :state (consult-lsp-symbols--state))))



(provide 'consult-lsp)
;;; consult-lsp.el ends here
