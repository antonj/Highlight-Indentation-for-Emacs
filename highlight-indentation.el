;;; highlight-indentation.el --- Minor modes for highlighting indentation
;; Author: Anton Johansson <anton.johansson@gmail.com> - http://antonj.se
;; Created: Dec 15 23:42:04 2010
;; Version: 0.7.0
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
  '((t :inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

(defcustom highlight-indentation-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "Default indentation offset, used if no other can be found from
  major mode. This value is always used by
  `highlight-indentation-mode' if set buffer local. Set buffer
  local with `highlight-indentation-set-offset'"
  :type 'integer
  :group 'highlight-indentation)

(defcustom highlight-indentation-blank-lines nil
  "Show indentation guides on blank lines.  Experimental.

Known issues:
- Doesn't work well with completion popups that use overlays
- Overlays on blank lines sometimes aren't cleaned up or updated perfectly
  Can be refreshed by scrolling
- Not yet implemented for highlight-indentation-current-column-mode
- May not work perfectly near the bottom of the screen
- Point appears after indent guides on blank lines"
  :type 'boolean
  :group 'highlight-indentation)

(defvar highlight-indentation-overlay-priority 1)
(defvar highlight-indentation-current-column-overlay-priority 2)

(defconst highlight-indentation-hooks
  '((after-change-functions (lambda (start end length)
                              (highlight-indentation-redraw-region
                               start end
                               'highlight-indentation-overlay
                               'highlight-indentation-put-overlays-region))
                            t t)
    (window-scroll-functions (lambda (win start)
                               (highlight-indentation-redraw-window
                                win
                                'highlight-indentation-overlay
                                'highlight-indentation-put-overlays-region
                                start))
                             nil t)))

(defun highlight-indentation-get-buffer-windows (&optional all-frames)
  "Return a list of windows displaying the current buffer."
  (get-buffer-window-list (current-buffer) 'no-minibuf all-frames))

(defun highlight-indentation-delete-overlays-buffer (overlay)
  "Delete all overlays in the current buffer."
  (save-restriction
    (widen)
    (highlight-indentation-delete-overlays-region (point-min) (point-max) overlay)))

(defun highlight-indentation-delete-overlays-region (start end overlay)
  "Delete overlays between START and END."
  (mapc #'(lambda (o)
            (if (overlay-get o overlay) (delete-overlay o)))
        (overlays-in start end)))

(defun highlight-indentation-redraw-window (win overlay func &optional start)
  "Redraw win starting from START."
  (highlight-indentation-redraw-region (or start (window-start win)) (window-end win t) overlay func))

(defun highlight-indentation-redraw-region (start end overlay func)
  "Erase and read overlays between START and END."
  (save-match-data
    (save-excursion
      (let ((inhibit-point-motion-hooks t)
            (start (save-excursion (goto-char start) (beginning-of-line) (point)))

            (end (save-excursion (goto-char end) (line-beginning-position 2))))
        (highlight-indentation-delete-overlays-region start end overlay)
        (funcall func start end overlay)))))

(defun highlight-indentation-redraw-all-windows (overlay func &optional all-frames)
  "Redraw the all windows showing the current buffer."
  (dolist (win (highlight-indentation-get-buffer-windows all-frames))
    (highlight-indentation-redraw-window win overlay func)))

(defun highlight-indentation-put-overlays-region (start end overlay)
  "Place overlays between START and END."
  (goto-char end)
  (let (o ;; overlay
        (last-indent 0)
        (last-char 0)
        (pos (point))
        (loop t))
    (while (and loop
                (>= pos start))
      (save-excursion
        (beginning-of-line)
        (let ((c 0)
              (cur-column (current-column)))
          (while (and (setq c (char-after))
                      (integerp c)
                      (not (= 10 c)) ;; newline
                      (= 32 c)) ;; space
            (when (= 0 (% cur-column highlight-indentation-offset))
              (let ((p (point)))
                (setq o (make-overlay p (+ p 1))))
              (overlay-put o overlay t)
              (overlay-put o 'priority highlight-indentation-overlay-priority)
              (overlay-put o 'face 'highlight-indentation-face))
            (forward-char)
            (setq cur-column (current-column)))
          (when (and highlight-indentation-blank-lines
                     (integerp c)
                     (or (= 10 c)
                         (= 13 c)))
            (when (< cur-column last-indent)
              (let ((column cur-column)
                    (s nil)
                    (show t)
                    num-spaces)
                (while (< column last-indent)
                  (if (>= 0
                          (setq num-spaces
                                (%
                                 (- last-indent column)
                                 highlight-indentation-offset)))
                      (progn
                        (setq num-spaces (1- highlight-indentation-offset))
                        (setq show t))
                    (setq show nil))
                  (setq s (cons (concat
                                 (if show
                                     (propertize " "
                                                 'face
                                                 'highlight-indentation-face)
                                   "")
                                 (make-string num-spaces 32))
                                s))
                  (setq column (+ column num-spaces (if show 1 0))))
                (setq s (apply 'concat (reverse s)))
                (let ((p (point)))
                  (setq o (make-overlay p p)))
                (overlay-put o overlay t)
                (overlay-put o 'priority highlight-indentation-overlay-priority)
                (overlay-put o 'after-string s))
              (setq cur-column last-indent)))
          (setq last-indent (* highlight-indentation-offset
                               (ceiling (/ (float cur-column)
                                           highlight-indentation-offset))))))
      (when (= pos start)
        (setq loop nil))
      (forward-line -1) ;; previous line
      (setq pos (point)))))

(defun highlight-indentation-guess-offset ()
  "Get indentation offset of current buffer."
  (cond ((and (eq major-mode 'python-mode) (boundp 'python-indent))
         python-indent)
        ((and (eq major-mode 'python-mode) (boundp 'py-indent-offset))
         py-indent-offset)
        ((and (eq major-mode 'python-mode) (boundp 'python-indent-offset))
         python-indent-offset)
        ((and (eq major-mode 'ruby-mode) (boundp 'ruby-indent-level))
         ruby-indent-level)
        ((and (eq major-mode 'scala-mode) (boundp 'scala-indent:step))
         scala-indent:step)
        ((and (eq major-mode 'scala-mode) (boundp 'scala-mode-indent:step))
         scala-mode-indent:step)
        ((and (or (eq major-mode 'scss-mode) (eq major-mode 'css-mode)) (boundp 'css-indent-offset))
         css-indent-offset)
        ((and (eq major-mode 'nxml-mode) (boundp 'nxml-child-indent))
         nxml-child-indent)
        ((and (eq major-mode 'coffee-mode) (boundp 'coffee-tab-width))
         coffee-tab-width)
        ((and (eq major-mode 'js-mode) (boundp 'js-indent-level))
         js-indent-level)
        ((and (eq major-mode 'js2-mode) (boundp 'js2-basic-offset))
         js2-basic-offset)
        ((and (fboundp 'derived-mode-class) (eq (derived-mode-class major-mode) 'sws-mode) (boundp 'sws-tab-width))
         sws-tab-width)
        ((and (eq major-mode 'web-mode) (boundp 'web-mode-markup-indent-offset))
         web-mode-markup-indent-offset) ; other similar vars: web-mode-{css-indent,scripts}-offset
        ((and (eq major-mode 'web-mode) (boundp 'web-mode-html-offset)) ; old var
         web-mode-html-offset)
        ((and (local-variable-p 'c-basic-offset) (boundp 'c-basic-offset))
         c-basic-offset)
        ((and (eq major-mode 'yaml-mode) (boundp 'yaml-indent-offset))
         yaml-indent-offset)
        ((and (eq major-mode 'elixir-mode) (boundp 'elixir-smie-indent-basic))
         elixir-smie-indent-basic)
        (t
         (default-value 'highlight-indentation-offset))))

;;;###autoload
(define-minor-mode highlight-indentation-mode
  "Highlight indentation minor mode highlights indentation based on spaces"
  :lighter " ||"
  (when (not highlight-indentation-mode) ;; OFF
    (highlight-indentation-delete-overlays-buffer 'highlight-indentation-overlay)
    (dolist (hook highlight-indentation-hooks)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook))))

  (when highlight-indentation-mode ;; ON
    (when (not (local-variable-p 'highlight-indentation-offset))
      (set (make-local-variable 'highlight-indentation-offset)
           (highlight-indentation-guess-offset)))

    ;; Setup hooks
    (dolist (hook highlight-indentation-hooks)
      (apply 'add-hook hook))
    (highlight-indentation-redraw-all-windows 'highlight-indentation-overlay
                                              'highlight-indentation-put-overlays-region)))

;;;###autoload
(defun highlight-indentation-set-offset (offset)
  "Set indentation offset locally in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode"
  (interactive
   (if (and current-prefix-arg (not (consp current-prefix-arg)))
       (list (prefix-numeric-value current-prefix-arg))
     (list (read-number "Indentation offset: "))))
  (set (make-local-variable 'highlight-indentation-offset) offset)
  (when highlight-indentation-mode
    (highlight-indentation-mode)))

;;; This minor mode will highlight the indentation of the current line
;;; as a vertical bar (grey background color) aligned with the column of the
;;; first character of the current line.
(defface highlight-indentation-current-column-face
  ;; Fringe has non intrusive color in most color-themes
  '((t (:background "black")))
  "Basic face for highlighting indentation guides."
  :group 'highlight-indentation)

(defconst highlight-indentation-current-column-hooks
  '((post-command-hook (lambda ()
                         (highlight-indentation-redraw-all-windows 'highlight-indentation-current-column-overlay
                                                                   'highlight-indentation-current-column-put-overlays-region)) nil t)))

(defun highlight-indentation-current-column-put-overlays-region (start end overlay)
  "Place overlays between START and END."
  (let (o ;; overlay
        (last-indent 0)
        (indent (save-excursion (back-to-indentation) (current-column)))
        (pos start))
    (goto-char start)
    ;; (message "doing it %d" indent)
    (while (< pos end)
      (beginning-of-line)
      (while (and (integerp (char-after))
                  (not (= 10 (char-after))) ;; newline
                  (= 32 (char-after))) ;; space
        (when (= (current-column) indent)
          (setq pos (point)
                last-indent pos
                o (make-overlay pos (+ pos 1)))
          (overlay-put o overlay t)
          (overlay-put o 'priority highlight-indentation-current-column-overlay-priority)
          (overlay-put o 'face 'highlight-indentation-current-column-face))
        (forward-char))
      (forward-line) ;; Next line
      (setq pos (point)))))

;;;###autoload
(define-minor-mode highlight-indentation-current-column-mode
  "Highlight Indentation minor mode displays a vertical bar
corresponding to the indentation of the current line"
  :lighter " |"

  (when (not highlight-indentation-current-column-mode) ;; OFF
    (highlight-indentation-delete-overlays-buffer 'highlight-indentation-current-column-overlay)
    (dolist (hook highlight-indentation-current-column-hooks)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook))))

  (when highlight-indentation-current-column-mode ;; ON
    (when (not (local-variable-p 'highlight-indentation-offset))
      (set (make-local-variable 'highlight-indentation-offset)
           (highlight-indentation-guess-offset)))

    ;; Setup hooks
    (dolist (hook highlight-indentation-current-column-hooks)
      (apply 'add-hook hook))
    (highlight-indentation-redraw-all-windows 'highlight-indentation-current-column-overlay
                                              'highlight-indentation-current-column-put-overlays-region)))

(provide 'highlight-indentation)

;;; highlight-indentation.el ends here
