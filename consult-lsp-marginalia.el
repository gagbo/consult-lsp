;;; consult-lsp-marginalia.el --- Marginalia support for consult-lsp -*- lexical-binding: t; -*-

;; Licence: MIT
;; Author: Gerry Agbobada
;; Maintainer: Gerry Agbobada
;; Package-Requires: ((emacs "26.3") (consult-lsp "0.6") (marginalia "0.8"))
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
;; Integrate Marginalia mode with consult-lsp
;;
;; The global minor mode consult-lsp-marginalia-mode replaces the annotations
;; of consult-lsp commands to make use of marginalia
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'marginalia)
(require 'consult-lsp)

(defun consult-lsp-marginalia--diagnostics-annotate-builder ()
  "Annotation function for `consult-lsp-diagnostics' making use of marginalia.

See `consult-lsp--diagnostics--transformer' for the usable text-properties
in candidates."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen)))))
    (lambda (cand)
      (let* ((diag (cdr (get-text-property 0 'consult--candidate cand))))
        (marginalia--fields
         ((consult-lsp--diagnostics--severity-to-level diag) :width 4)
         ((replace-regexp-in-string "\n" " " (lsp:diagnostic-message diag))
          :truncate marginalia-truncate-width)
         ((or (consult-lsp--diagnostics--source diag) "")
          :face 'font-lock-doc-face :width 10))))))

(defun consult-lsp-marginalia--symbols-annotate-builder ()
  "Annotation function for `consult-lsp-symbols' making use of marginalia.

See `consult-lsp--symbols--transformer' for the available text-properties
usable in the annotation-function."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (fmt (propertize (format "%%%dd" width) 'face 'consult-line-number-prefix)))
    (lambda (cand)
      (let* ((symbol-info (get-text-property 0 'consult--candidate cand))
             (line (thread-first symbol-info
                     (lsp:symbol-information-location)
                     (lsp:location-range)
                     (lsp:range-start)
                     (lsp:position-line)
                     (1+)
                     (lsp-translate-line))))
        (marginalia--fields
         ((alist-get (lsp:symbol-information-kind symbol-info) lsp-symbol-kinds) :width 7)
         ((or (get-text-property 0 'consult--details cand) "")
          :face 'font-lock-doc-face :truncate (/ marginalia-truncate-width 2)))))))

(defun consult-lsp-marginalia--file-symbols-annotate-builder ()
  "Annotation function for `consult-lsp-file-symbols' making use of marginalia."
  (let* ((width (length (number-to-string (line-number-at-pos
                                           (point-max)
                                           consult-line-numbers-widen))))
         (fmt (propertize (format "%%%dd" width) 'face 'consult-line-number-prefix)))
    (lambda (cand)
      (let ((line (cdr (get-text-property 0 'consult-location cand))))
        (marginalia--fields
         ((format fmt line))
         ((or (get-text-property 0 'consult--details cand) "")
          :face 'font-lock-doc-face :truncate (/ marginalia-truncate-width 2)))))))

(defvar old-consult-lsp-diagnostics-annotate-builder-function)
(defvar old-consult-lsp-symbols-annotate-builder-function)
(defvar old-consult-lsp-file-symbols-annotate-builder-function)

;;;###autoload
(define-minor-mode consult-lsp-marginalia-mode
  "Use marginalia with consult-lsp functions."
  :global t
  (if consult-lsp-marginalia-mode
      (progn
        (setq
         old-consult-lsp-diagnostics-annotate-builder-function consult-lsp-diagnostics-annotate-builder-function
         old-consult-lsp-symbols-annotate-builder-function consult-lsp-symbols-annotate-builder-function
         old-consult-lsp-file-symbols-annotate-builder-function consult-lsp-file-symbols-annotate-builder-function
         consult-lsp-diagnostics-annotate-builder-function #'consult-lsp-marginalia--diagnostics-annotate-builder
         consult-lsp-symbols-annotate-builder-function #'consult-lsp-marginalia--symbols-annotate-builder
         consult-lsp-file-symbols-annotate-builder-function #'consult-lsp-marginalia--file-symbols-annotate-builder)

        (add-to-list 'marginalia-annotator-registry
                     `(consult-lsp-diagnostics ,(consult-lsp-marginalia--diagnostics-annotate-builder) builtin none))
        (add-to-list 'marginalia-annotator-registry
                     `(consult-lsp-symbols ,(consult-lsp-marginalia--symbols-annotate-builder) builtin none))
        (add-to-list 'marginalia-annotator-registry
                     `(consult-lsp-file-symbols ,(consult-lsp-marginalia--file-symbols-annotate-builder) builtin none)))
    (setq
     consult-lsp-diagnostics-annotate-builder-function old-consult-lsp-diagnostics-annotate-builder-function
     consult-lsp-symbols-annotate-builder-function old-consult-lsp-symbols-annotate-builder-function
     consult-lsp-file-symbols-annotate-builder-function old-consult-lsp-file-symbols-annotate-builder-function
     marginalia-annotator-registry (assq-delete-all 'consult-lsp-diagnostics marginalia-annotator-registry)
     marginalia-annotator-registry (assq-delete-all 'consult-lsp-symbols marginalia-annotator-registry)
     marginalia-annotator-registry (assq-delete-all 'consult-lsp-file-symbols marginalia-annotator-registry))))

(provide 'consult-lsp-marginalia)
;;; consult-lsp-marginalia.el ends here
