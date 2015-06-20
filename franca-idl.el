;;; franca-idl.el

;; Copyright (C) 2015 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Created: 16 Jun 2015

;; Keywords: languages
;; Homepage: http://github.com/zeph1e/franca-idl.el
;; License: WTFPL version 2, grab your copy here: http://www.wtfpl.net

;; This file is not part of GNU Emacs.

(defgroup franca-idl nil
"An emacs major mode which provides syntax highlighting and indentation for Franca IDL."
  :prefix "franca-idl-"
  :group 'languages
  :link '(url-link "https://github.com/zeph1e/franca-idl"))

(defvar franca-idl-mode-hook nil
  "Franca IDL mode hook.")

(defvar franca-idl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-e") 'franca-idl-export)
    (define-key map (kbd "{") 'franca-idl-open-brace)
    (define-key map (kbd "}") 'franca-idl-close-brace)
    map)
  "Keymap for Franca IDL mode.")

(defun franca-idl-open-brace (p)
  (interactive "p")
  (insert-char ?{)
  (franca-idl-indent-line t))

(defun franca-idl-close-brace (p)
  (interactive "p")
  (insert-char ?})
  (franca-idl-indent-line))
(defconst franca-idl-keywords '("array" "of"
                                "enumeration" "extends"
                                "struct"
                                "union"
                                "map" "to"
                                "typedef" "is"
                                "typeCollection"
                                "interface"
                                "version" "major" "minor"
                                "attribute" "readonly" "noSubscriptions"
                                "method" "in" "out" "error" "fireAndForget"
                                "broadcast" "selective"
                                "contract" "PSM" "initial" "state" "call" "signal" "on"
                                "package" "import" "model" "from"))

(defconst franca-idl-types '("UInt8" "Int8" "UInt16" "Int16" "UInt32" "Int32" "UInt64"
                             "Int64" "Boolean" "Float" "Double" "String" "ByteBuffer"))

;; (defconst franca-idl-annotations '("description"
;;                                    "author"
;;                                    "experimental"
;;                                    "deprecated"
;;                                    "see"
;;                                    "param"
;;                                    "high-volume"
;;                                    "high-frequency"
;;                                    "source-uri"
;;                                    "source-alias"
;;                                    "details"))

(defvar franca-idl-keywords-regexp (eval-when-compile (regexp-opt franca-idl-keywords 'words)))
(defvar franca-idl-types-regexp (eval-when-compile (regexp-opt franca-idl-types 'words)))
;; (defvar franca-idl-annotations-regexp (regexp-opt franca-idl-annotations 'words))

(defun franca-idl-indent-line (&optional ignore-current)
  "Indent current line as Franca IDL code."
  (interactive)
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (max (franca-idl-calculate-indentation ignore-current) 0))))
    (if (null indent-col) 'noindent
      (if savep
          (save-excursion (indent-line-to indent-col))
        (indent-line-to indent-col)))))

(defun franca-idl-calculate-indentation (&optional ignore-current)
  "Return the column to which the current line should be indented."
  (save-excursion
    (let ((curp (point))(paren 0) indent basep)
      (unless ignore-current
        (goto-char (min (1+ curp) (point-max)))) ; for current line
      (condition-case nil
          (while (search-backward-regexp "[{}]") ; find unmatching opening brace
            (if (char-equal ?{ (char-after))
                (progn
                  (setq paren (1+ paren)) ; opening brace
                  (if (> paren 0) (error "do break")))
              ;; if } is at the first column, don't go further unless user entered it
              (if (and (/= curp (point)) (= (line-beginning-position) (point)))
                  (error "no need to go further"))
              (setq paren (1- paren))))
        (error nil))
      (if (<= paren 0) 0 ; no unmatcing open brace found
        ;; calculate indent base position
        ;;   method aa {
        ;;   _ <-- base position
        ;; We need to find another open brace in same line for the case like:
        ;;   method aa { in {
        ;;                   _ <-- indent here
        (setq basep (search-backward-regexp "[{]" (point-at-bol) t))
        (if (null basep) (setq basep (point-at-bol))
          (setq basep (1+ basep)))
        (goto-char basep)
        (skip-chars-forward "\t ")
        (setq indent (current-column)) ; candidate
        ;; We need to check if there's following statements in the same line for the case like:
        ;;   method aa { in { String aa
        ;;                    _ <-- indent here
        (goto-char basep)
        (setq basep (search-forward-regexp "[{]\\s-*[^\t {}]" (point-at-eol) t))
        (if basep
          (- basep (line-beginning-position) 1)
            (+ indent tab-width))))))

(defvar franca-idl-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for Franca IDL mode.")

(defconst franca-idl-syntax-propertize-function
  (syntax-propertize-rules
   ("\\(<\\)\\*\\*" (1 "< c")) ("\\*\\*\\(>\\)" (1 "> c")))
   "Syntactic keywords for `franca-idl-mode'.")

;;;###autoload
(define-derived-mode franca-idl-mode prog-mode "Franca-IDL"
  "Major mode to help editing Franca IDL code.

Keybindings:
\\{franca-idl-mode-map}
"
  (interactive)
  (set-syntax-table franca-idl-syntax-table)
  (set (make-local-variable 'syntax-propertize-function) franca-idl-syntax-propertize-function)
  (use-local-map franca-idl-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults) '(franca-idl-font-lock-keywords))
  (set (make-local-variable 'font-lock-defaults)
       (list (list (cons franca-idl-keywords-regexp 'font-lock-keyword-face)
                   (cons franca-idl-types-regexp 'font-lock-type-face))))
  (set (make-local-variable 'indent-line-function) 'franca-idl-indent-line)
  (run-hooks 'franca-idl-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fidl\\'" . franca-idl-mode))

(provide 'franca-idl)
