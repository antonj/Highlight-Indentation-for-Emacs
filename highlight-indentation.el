;;; highlight-indentation.el --- Minor modes for highlighting indentation
;; Author: Anton Johansson <anton.johansson@gmail.com> - http://antonj.se
;; Created: Dec 15 23:42:04 2010
;; Version: 0.6.0
;; URL: https://github.com/antonj/Highlight-Indentation-for-Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;; Customize `highlight-indentation-face', and
;; `highlight-indentation-current-column-face' to suit your theme.

;;; Code:

(defgroup highlight-indentation nil
  "Highlight Indentation"
  :prefix "highlight-indentation-"
  :group 'basic-faces)

(defface highlight-indentation-face
  ;; Fringe has non intrusive color in most color-themes
  '((inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

(defcustom highlight-indentation-offset 4
  "Default indentation offset, used if no other can be found from
  major mode. This value is always used by
  `highlight-indentation-mode' if set buffer local. Set buffer
  local with `highlight-indentation-set-offset'"
  :group 'highlight-indentation)

(defconst highlight-indentation-hooks
  '((after-change-functions highlight-indentation-redraw-region t t)
    (window-scroll-functions highlight-indentation-update-window-for-scroll nil t)))

(defun highlight-indentation-get-buffer-windows (&optional all-frames)
  "Return a list of windows displaying the current buffer."
  (get-buffer-window-list (current-buffer) 'no-minibuf all-frames))

(defun highlight-indentation-delete-overlays-buffer ()
  "Delete all overlays in the current buffer."
  (save-restriction
    (widen)
    (highlight-indentation-delete-overlays-region (point-min) (point-max))))

(defun highlight-indentation-delete-overlays-region (start end)
  "Delete overlays between START and END."
  (mapc #'(lambda (o)
            (if (overlay-get o 'highlight-indentation-overlay) (delete-overlay o)))
        (overlays-in start end)))

(defun highlight-indentation-put-overlays-region (start end)
  "Place overlays between START and END."
  (goto-char start)
  (let (o ;; overlay
        (last-indent 0)
        (pos start))
    (while (< pos end)
      (beginning-of-line)
      (while (and (integerp (char-after))
                  (not (= 10 (char-after))) ;; newline
                  (= 32 (char-after))) ;; space
        (when (= 0 (% (current-column) highlight-indentation-offset))
          (setq pos (point)
                last-indent pos
                o (make-overlay pos (+ pos 1)))
          (overlay-put o 'highlight-indentation-overlay t)
          (overlay-put o 'face 'highlight-indentation-face))
        (forward-char))
      (forward-line) ;; Next line
      (setq pos (point)))))

(defun highlight-indentation-redraw-region (start end _ignored)
  "Erase and readd overlays between START and END."
  (save-match-data
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (goto-char end)
        (setq end (line-beginning-position 2))
        (highlight-indentation-delete-overlays-region start end)
        (highlight-indentation-put-overlays-region start end)))))

(defun highlight-indentation-redraw-window (win &optional start)
  "Redraw win starting from START."
  (highlight-indentation-redraw-region (or start (window-start win)) (window-end win t) 'ignored))

(defun highlight-indentation-redraw-all-windows (&optional all-frames)
  "Redraw the all windows showing the current buffer."
  (dolist (win (highlight-indentation-get-buffer-windows all-frames))
    (highlight-indentation-redraw-window win)))

(defun highlight-indentation-update-window-for-scroll (win start)
  "Redraw win"
  (highlight-indentation-redraw-window win start))

;;;###autoload
(define-minor-mode highlight-indentation-mode
  "Highlight indentation minor mode highlights indentation based on spaces"
  :lighter " ||"
  (when (not highlight-indentation-mode) ;; OFF
    (highlight-indentation-delete-overlays-buffer)
    (dolist (hook highlight-indentation-hooks)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook))))

  (when highlight-indentation-mode ;; ON
    (when (not (local-variable-p 'highlight-indentation-offset))
      (set (make-local-variable 'highlight-indentation-offset)
           ;; Set indentation offset from highlight-indentation-offset if set, otherwise
           ;; according to major mode
           (cond ((and (eq major-mode 'python-mode) (boundp 'python-indent))
                  python-indent)
                 ((and (eq major-mode 'python-mode) (boundp 'py-indent-offset))
                  py-indent-offset)
                 ((and (eq major-mode 'python-mode) (boundp 'python-indent-offset))
                  python-indent-offset)
                 ((eq major-mode 'ruby-mode)
                  ruby-indent-level)
                 ((and (eq major-mode 'scala-mode) (boundp 'scala-indent:step))
                  scala-indent:step)
                 ((and (eq major-mode 'scala-mode) (boundp 'scala-mode-indent:step))
                  scala-mode-indent:step)
                 ((or (eq major-mode 'scss-mode) (eq major-mode 'css-mode))
                  css-indent-offset)
                 ((eq major-mode 'nxml-mode)
                  nxml-child-indent)
                 ((eq major-mode 'coffee-mode)
                  coffee-tab-width)
                 ((eq major-mode 'js-mode)
                  js-indent-level)
                 ((eq major-mode 'js2-mode)
                  js2-basic-offset)
                 ((and (fboundp 'derived-mode-class) (eq (derived-mode-class major-mode) 'sws-mode))
                  sws-tab-width)
                 ((eq major-mode 'web-mode)
                  web-mode-html-offset) ; other similar vars: web-mode-{css-indent,scripts}-offset
                 ((local-variable-p 'c-basic-offset)
                  c-basic-offset)
                 (t
                  (default-value 'highlight-indentation-offset)))))

    ;; Setup hooks
    (dolist (hook highlight-indentation-hooks)
      (apply 'add-hook hook))
    (highlight-indentation-redraw-all-windows)))

;;;###autoload
(defun highlight-indentation-set-offset (offset)
  "Set indentation offset localy in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode"
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Indentation offset: "))))
  (set (make-local-variable 'highlight-indentation-offset) offset)
  (when highlight-indentation-mode
    (highlight-indentation-mode)))


;;;
;;; Copyright (C) Kresten Krab Thorup
;;; Available under Apache License, Version 2.
;;;
;;; This minor mode will highlight the indentation of the current line
;;; as a vertical bar (grey background color) aligned with the column of the
;;; first character of the current line.
;;;
(defface highlight-indentation-current-column-face
  ;; Fringe has non intrusive color in most color-themes
  '((t :inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

;; used to hold the last regex we installed
(defvar highlight-indentation-current-column-regex nil)

;;;###autoload
(define-minor-mode
  highlight-indentation-current-column-mode
  "Hilight Indentation minor mode displays
a vertical bar corresponding to the indentation of the current line"
  :lighter " |"

  (when highlight-indentation-current-column-regex
    (font-lock-remove-keywords nil highlight-indentation-current-column-regex))

  (set (make-local-variable 'highlight-indentation-current-column-regex) nil)
  (cond  (highlight-indentation-current-column-mode
          (add-hook 'post-command-hook 'highlight-indentation-current-column-post-command-hook nil t))
         (t
          (remove-hook 'post-command-hook 'highlight-indentation-current-column-post-command-hook t)

          (font-lock-fontify-buffer))))

(defun highlight-indentation-current-column-post-command-hook ()
  "This hook runs after every keystroke"
  (when highlight-indentation-current-column-regex
    (font-lock-remove-keywords nil highlight-indentation-current-column-regex))
  (let ((indent (save-excursion (back-to-indentation) (current-column))))
    (when (and highlight-indentation-current-column-mode
               (> indent 1))
      (let* ((re (format "^ \\{%d\\}\\( \\)" indent))
             (arg `((,re (1 'highlight-indentation-current-column-face prepend)))))
        (set (make-local-variable 'highlight-indentation-current-column-regex) arg)
        (font-lock-add-keywords nil arg))))
  (font-lock-fontify-buffer))

(provide 'highlight-indentation)

;;; highlight-indentation.el ends here
