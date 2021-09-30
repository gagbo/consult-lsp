;;; consult-lsp.el --- LSP-mode Consult integration -*- lexical-binding: t; -*-

;; Licence: MIT
;; Keywords: tools, completion, lsp
;; Author: Gerry Agbobada
;; Maintainer: Gerry Agbobada
;; Package-Requires: ((emacs "27.1") (lsp-mode "5.0") (consult "0.9") (f "0.20.0"))
;; Version: 0.6
;; Homepage: https://github.com/gagbo/consult-lsp

;; Copyright (c) 2021 Gerry Agbobada and contributors
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
;;
;; The commands are autoloaded so you don't need to require anything to make them
;; available. Just use M-x and go!
;;
;;;; Diagnostics
;; M-x consult-lsp-diagnostics provides a view of all diagnostics in the current
;; workspace (or all workspaces if passed a prefix argument).
;;
;; You can use prefixes to filter diagnostics per severity, and
;; previewing/selecting a candidate will go to it directly.
;;
;;;; Symbols
;; M-x consult-lsp-symbols provides a selection/narrowing command to search
;; and go to any arbitrary symbol in the workspace (or all workspaces if
;; passed a prefix argument).
;;
;; You can use prefixes as well to filter candidates per type, and
;; previewing/selecting a candidate will go to it.
;;
;;;; File symbols
;; M-x consult-lsp-file-symbols provides a selection/narrowing command to search
;; and go to any arbitrary symbol in the selected buffer (like imenu)
;;
;;;; Contributions
;; Possible contributions for ameliorations include:
;; - using a custom format for the propertized candidates
;;   This should be done with :type 'function custom variables probably.
;; - checking the properties in LSP to see how diagnostic-sources should be used
;; - checking the properties in LSP to see how symbol-sources should be used
;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'consult)
(require 'lsp)
(require 'f)
(require 'cl-lib)

(defgroup consult-lsp nil
  "Consult commands for `lsp-mode'."
  :group 'tools
  :prefix "consult-lsp-")

;;;; Customization
(defcustom consult-lsp-diagnostics-transformer-function #'consult-lsp--diagnostics--transformer
  "Function that transform LSP-mode diagnostic from a (file diag) pair to a candidate for `consult-lsp-diagnostics'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-diagnostics-annotate-builder-function #'consult-lsp--diagnostics-annotate-builder
  "Annotation function builder for `consult-lsp-diagnostics'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-symbols-transformer-function #'consult-lsp--symbols--transformer
  "Function that transform LSP symbols from symbol-info to a candidate for `consult-lsp-symbols'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-symbols-annotate-builder-function #'consult-lsp--symbols-annotate-builder
  "Annotation function builder for `consult-lsp-symbols'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-file-symbols-transformer-function #'consult-lsp--file-symbols--transformer
  "Function that transform LSP file symbols from symbol-info to a candidate for `consult-lsp-file-symbols'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-file-symbols-annotate-builder-function #'consult-lsp--file-symbols-annotate-builder
  "Annotation function builder for `consult-lsp-file-symbols'."
  :type 'function
  :group 'consult-lsp)

(defcustom consult-lsp-symbols-narrow
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
    ;; Example types included in "Other" (i.e. the ignored)
    ;; (?n . "Null")
    ;; (?c . "Constructor")
    ;; (?e . "Event")
    ;; (?k . "Key")
    ;; (?o . "Operator")
    )
  "Set of narrow keys for `consult-lsp-symbols' and `consult-lsp-file-symbols'.

It MUST have a \"Other\" category for everything that is not listed."
  :group 'consult-lsp
  :type '(alist :key-type character :value-type string))



;;;; Diagnostics

(defun consult-lsp--diagnostics--flatten-diagnostics (transformer &optional current-workspace?)
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

(defun consult-lsp--diagnostics--severity-to-level (diag)
  "Convert diagnostic severity of DIAG to a string."
  (pcase (lsp:diagnostic-severity? diag)
    (1 (propertize "error" 'face 'error))
    (2 (propertize "warn" 'face 'warning))
    (3 (propertize "info" 'face 'success))
    (4 (propertize "hint" 'face 'italic))
    (_ "unknown")))

(defconst consult-lsp--diagnostics--narrow
  '((?e . "Errors")
    (?w . "Warnings")
    (?i . "Infos")
    (?h . "Hints")
    (?u . "Unknown"))
  "Set of narrow keys for `consult-lsp-diagnostics'.")

(defun consult-lsp--diagnostics--severity-to-type (diag)
  "Convert diagnostic severity of DIAG to a type for consult--type."
  (pcase (lsp:diagnostic-severity? diag)
    (1 (car (rassoc "Errors" consult-lsp--diagnostics--narrow)))
    (2 (car (rassoc "Warnings" consult-lsp--diagnostics--narrow)))
    (3 (car (rassoc "Infos" consult-lsp--diagnostics--narrow)))
    (4 (car (rassoc "Hints" consult-lsp--diagnostics--narrow)))
    (_ (car (rassoc "Unknown" consult-lsp--diagnostics--narrow)))))

(defun consult-lsp--diagnostics--source (diag)
  "Convert source of DIAG to a propertized string."
  (propertize (lsp:diagnostic-source? diag) 'face 'success))

(defun consult-lsp--diagnostics--diagnostic-marker (file diag)
  "Return a marker in FILE at the beginning of DIAG."
  (consult--position-marker
   file
   (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range diag)))))
   (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range diag)))))))

(defun consult-lsp--diagnostics--transformer (file diag)
  "Transform LSP-mode diagnostics from a pair FILE DIAG to a candidate."
  (propertize
   (format "%-60.60s"
           (consult--format-location
            (if-let ((wks (lsp-workspace-root file)))
                (f-relative file wks)
              file)
            (lsp-translate-line (1+ (lsp-get (lsp-get (lsp-get diag :range) :start) :line)))))
   'consult--candidate (cons file diag)
   'consult--type (consult-lsp--diagnostics--severity-to-type diag)))

(defun consult-lsp--diagnostics-annotate-builder ()
  "Annotation function for `consult-lsp-diagnostics'.

See `consult-lsp--diagnostics--transformer' for the usable text-properties
in candidates."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen)))))
    (lambda (cand)
      (let* ((diag (cdr (get-text-property 0 'consult--candidate cand))))
        (list cand
              (consult-lsp--diagnostics--severity-to-level diag)
              (concat
               (format "%s %s"
                       (consult-lsp--diagnostics--severity-to-level diag)
                       (replace-regexp-in-string "\n" " " (lsp:diagnostic-message diag)))
               (when-let ((source (consult-lsp--diagnostics--source diag)))
                 (propertize (format " - %s" source) 'face 'font-lock-doc-face))))))))

(defun consult-lsp--diagnostics--state ()
  "LSP diagnostic preview."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (cond
       (cand
        (funcall jump
                 (consult--position-marker
                  (and (car cand) (funcall (if restore #'find-file open) (car cand)))
                  (lsp-translate-line (1+ (lsp:position-line (lsp:range-start (lsp:diagnostic-range (cdr cand))))))
                  (lsp-translate-column (1+ (lsp:position-character (lsp:range-start (lsp:diagnostic-range (cdr cand)))))))
                 restore))
       (restore
        (funcall jump nil t)
        (funcall open nil))))))

;;;###autoload
(defun consult-lsp-diagnostics (arg)
  "Query LSP-mode diagnostics. When ARG is set through prefix, query all workspaces."
  (interactive "P")
  (let ((all-workspaces? arg))
    (consult--read (consult-lsp--diagnostics--flatten-diagnostics consult-lsp-diagnostics-transformer-function (not all-workspaces?))
                   :prompt (concat  "LSP Diagnostics " (when arg "(all workspaces) "))
                   :annotate (funcall consult-lsp-diagnostics-annotate-builder-function)
                   :require-match t
                   :history t
                   :category 'consult-lsp-diagnostics
                   :sort nil
                   :group (consult--type-group consult-lsp--diagnostics--narrow)
                   :narrow (consult--type-narrow consult-lsp--diagnostics--narrow)
                   :state (consult-lsp--diagnostics--state)
                   :lookup #'consult--lookup-candidate)))


;;;; Symbols

(defun consult-lsp--symbols--kind-to-narrow (symbol-info)
  "Get the narrow character for SYMBOL-INFO."
  (if-let ((pair (rassoc
                  (alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds)
                  consult-lsp-symbols-narrow)))
      (car pair)
    (rassoc "Other" consult-lsp-symbols-narrow)))

(defun consult-lsp--symbols--state ()
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
(defun consult-lsp--symbols--make-async-source (async workspaces)
  "Pipe a `consult--read' compatible async-source ASYNC to search for symbols in WORKSPACES."
  (let ((cancel-token :consult-lsp--symbols))
    (lambda (action)
      (pcase-exhaustive action
        ((or 'setup (pred stringp))
         (let ((query (if (stringp action) action "")))
           (with-lsp-workspaces workspaces
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
              :cancel-token cancel-token)))
         (funcall async action))
        ('destroy
         (lsp-cancel-request-by-token cancel-token)
         (funcall async action))
        (_ (funcall async action))))))

(defun consult-lsp--symbols--transformer (symbol-info)
  "Default transformer to produce a completion candidate from SYMBOL-INFO."
  (propertize
   ;; We have to add the location in the candidate string for 2 purposes,
   ;; in case symbols have the same name:
   ;; - being able to narrow using the path
   ;; - because it breaks marginalia integration otherwise
   ;;   (it uses a cache where candidates are caching keys through `marginalia--cached')
   (format "%s — %s"
           (lsp:symbol-information-name symbol-info)
           (consult--format-location
            (let ((file
                   (lsp--uri-to-path (lsp:location-uri (lsp:symbol-information-location symbol-info)))))
              (if-let ((wks (lsp-workspace-root file)))
                  (f-relative file wks)
                file))
            (thread-first symbol-info
              (lsp:symbol-information-location)
              (lsp:location-range)
              (lsp:range-start)
              (lsp:position-line)
              (1+)
              (lsp-translate-line))))
   'consult--type (consult-lsp--symbols--kind-to-narrow symbol-info)
   'consult--candidate symbol-info
   'consult--details (lsp:document-symbol-detail? symbol-info)))

(defun consult-lsp--symbols-annotate-builder ()
  "Annotation function for `consult-lsp-symbols'.

See `consult-lsp--symbols--transformer' for the available text-properties
usable in the annotation-function."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (fmt (propertize (format "%%%dd " width) 'face 'consult-line-number-prefix)))
    (lambda (cand)
      (let* ((symbol-info (get-text-property 0 'consult--candidate cand))
             (line (thread-first symbol-info
                     (lsp:symbol-information-location)
                     (lsp:location-range)
                     (lsp:range-start)
                     (lsp:position-line)
                     (1+)
                     (lsp-translate-line))))
        (list
         cand
         (format " %-7s"
                 (alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds))
         (concat
          (or
           (when-let ((details (get-text-property 0 'consult--details cand)))
             (propertize (format " — %s" details) 'face 'font-lock-doc-face))
           "")))))))

;;;###autoload
(defun consult-lsp-symbols (arg)
  "Query workspace symbols. When ARG is set through prefix, query all workspaces."
  (interactive "P")
  (let* ((initial "")
         (all-workspaces? arg)
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
       (consult--async-map consult-lsp-symbols-transformer-function)
       (consult-lsp--symbols--make-async-source ws)
       (consult--async-throttle)
       (consult--async-split))
     :prompt "LSP Symbols "
     :annotate (funcall consult-lsp-symbols-annotate-builder-function)
     :require-match t
     :history t
     :initial (consult--async-split-initial initial)
     :category 'consult-lsp-symbols
     :lookup #'consult--lookup-candidate
     :group (consult--type-group consult-lsp-symbols-narrow)
     :narrow (consult--type-narrow consult-lsp-symbols-narrow)
     :state (consult-lsp--symbols--state))))


;;;; File symbols

(defun consult-lsp--flatten-document-symbols (to-flatten)
  "Helper function for flattening document symbols to a plain list."
  (cl-labels ((rec-helper
               (to-flatten accumulator)
               (dolist (table to-flatten)
                 (when (hash-table-p table)
                   (push table accumulator)
                   (when-let ((children (gethash "children" table)))
                     (setq accumulator (rec-helper
                                        (append children nil) ; convert children from vector to list
                                        accumulator))
                     (remhash "children" table))))
               accumulator))
    (nreverse (rec-helper to-flatten nil))))

(defun consult-lsp--file-symbols--transformer (symbol)
  "Default transformer to produce a completion candidate from SYMBOL."
  (let ((line (thread-first symbol
                (lsp:document-symbol-selection-range)
                (lsp:range-start)
                (lsp:position-line)
                (lsp-translate-line)))
        (cbeg (thread-first symbol
                (lsp:document-symbol-selection-range)
                (lsp:range-start)
                (lsp:position-character)
                (lsp-translate-column)))
        (cend (thread-first symbol
                (lsp:document-symbol-selection-range)
                (lsp:range-end)
                (lsp:position-character)
                (lsp-translate-column))))
    (let ((beg (lsp--line-character-to-point line cbeg))
          (end (lsp--line-character-to-point line cend))
          (marker (make-marker)))
      (set-marker marker beg)
      ;; Pre-condition to respect narrowing
      (unless (or (< beg (point-min))
                  (> end (point-max)))
        ;; NOTE: no need to add anything to the candidate string like
        ;; for consult-lsp-symbols because
        ;; - we have the line location and there are less hits in this command,
        ;; - the candidates are different caching keys because of
        ;;   `consult--location-candidate' usage.
        ;;
        ;; `consult--location-candidate' is unavailable for
        ;; `consult-lsp--symbols--transformer'because it needs a marker,
        ;; and we cannot create marker for buffers that aren't open.
        (consult--location-candidate
         (let ((substr (consult--buffer-substring beg end 'fontify))
               (symb-info-name (lsp:symbol-information-name symbol)))
           (concat substr
                   (unless (string= substr symb-info-name)
                     (format " (%s)"
                             symb-info-name))))
         marker
         (1+ line)
         'consult--type (consult-lsp--symbols--kind-to-narrow symbol)
         'consult--name (lsp:symbol-information-name symbol)
         'consult--details (lsp:document-symbol-detail? symbol))))))

(defun consult-lsp--file-symbols-candidates ()
  "Returns all candidates for a `consult-lsp-file-symbols' search.

See the :annotate documentation of `consult--read' for more information."
  (consult--forbid-minibuffer)
  (let* ((all-symbols (consult-lsp--flatten-document-symbols
                       (lsp-request "textDocument/documentSymbol"
                                    (lsp-make-document-symbol-params :text-document
                                                                     (lsp--text-document-identifier)))))
         (candidates (mapcar consult-lsp-file-symbols-transformer-function all-symbols)))
    (unless candidates
      (user-error "No symbols"))
    candidates))

(defun consult-lsp--file-symbols-annotate-builder ()
  "Annotation function for `consult-lsp-file-symbols'."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (fmt (propertize (format "%%%dd " width) 'face 'consult-line-number-prefix)))
    (lambda (cand)
      (let ((line (cdr (get-text-property 0 'consult-location cand))))
        (list cand
              (format fmt line)
              (concat
               (when-let ((details (get-text-property 0 'consult--details cand)))
                 (propertize (format " - %s" details) 'face 'font-lock-doc-face))))))))

;;;###autoload
(defun consult-lsp-file-symbols ()
  "Search symbols defined in current file in a manner similar to `consult-line'."
  (interactive)
  (consult--read
   (consult--with-increased-gc (consult-lsp--file-symbols-candidates))
   :prompt "Go to symbol: "
   :annotate (funcall consult-lsp-file-symbols-annotate-builder-function)
   :require-match t
   :sort nil
   :history '(:input consult--line-history)
   :category 'consult-lsp-file-symbols
   :lookup #'consult--line-match
   :narrow (consult--type-narrow consult-lsp-symbols-narrow)
   :state (consult--jump-state)))



(provide 'consult-lsp)
;;; consult-lsp.el ends here
