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
;; Customize `highlight-indentation-face' to suit your theme.

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

(defcustom highlight-indentation-offset 4
  "Default indentation offset, used if no other can be found from
  major mode. This value is always used by
  `highlight-indentation-mode' if set buffer local. Set buffer
  local with `highlight-indentation-set-offset'"
  :group 'highlight-indendation)

(defvar highlight-indentation-current-regex nil)

;;;###autoload
(define-minor-mode highlight-indentation-mode
  "Highlight indentation minor mode highlights indentation based
on spaces"
  :lighter " ||"
  (when highlight-indentation-current-regex ;; OFF
    (font-lock-remove-keywords nil `((,highlight-indentation-current-regex
                                      (1 'highlight-indentation-face)))))

  (set (make-local-variable 'highlight-indentation-current-regex) nil)

  (when highlight-indentation-mode ;; ON
    (set (make-local-variable 'highlight-indentation-offset)
         ;; Set indentation offset from highlight-indentation-offset if set, otherwise
         ;; according to major mode
         (cond ((local-variable-p 'highlight-indentation-offset)
                highlight-indentation-offset)
               ((eq major-mode 'python-mode)
                (if (boundp 'python-indent)
                    python-indent
                  py-indent-offset))
               ((eq major-mode 'ruby-mode)
                ruby-indent-level)
               ((eq major-mode 'nxml-mode)
                nxml-child-indent)
               ((local-variable-p 'c-basic-offset)
                c-basic-offset)
               (t
                (default-value 'highlight-indentation-offset))))
    (set (make-local-variable 'highlight-indentation-current-regex)
         (format "\\( \\) \\{%s\\}" (- highlight-indentation-offset 1)))
    (font-lock-add-keywords nil `((,highlight-indentation-current-regex
                                   (1 'highlight-indentation-face)))))
  (font-lock-fontify-buffer))

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

(provide 'highlight-indentation)
                      
;;; highlight-indentation.el ends here
               
