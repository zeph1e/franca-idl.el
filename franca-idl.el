;;; franca-idl-mode.el

(defgroup 'franca-idl-mode
  :prefix "franca-idl-"
  :group 'languages)

(defvar franca-idl-mode-hook nil
  "Franca IDL mode hook.")

(defvar franca-idl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-e") 'franca-idl-export)
    map)
  "Keymap for Franca IDL mode.")

;; (defconst franca-idl-keywords '("array" "of"
;;                                 "enumeration" "extends"
;;                                 "struct"
;;                                 "union"
;;                                 "map" "to"
;;                                 "typedef" "is"
;;                                 "typeCollection"
;;                                 "interface"
;;                                 "version" "major" "minor"
;;                                 "attribute" "readonly" "noSubscriptions"
;;                                 "method" "in" "out" "error" "fireAndForget"
;;                                 "broadcast" "selective"
;;                                 "contract" "PSM" "initial" "state" "call" "signal" "on"
;;                                 "package" "import" "model" "from"))
;; (defconst franca-idl-types '("UInt8" "Int8" "UInt16" "Int16" "UInt32" "Int32" "UInt64"
;;                              "Int64" "Boolean" "Float" "Double" "String" "ByteBuffer"))

;; (defconst franca-idl-keywords-regexp (regexp-opt franca-idl-keywords 'words))
;; (defconst franca-idl-types-regexp (regexp-opt franca-idl-types 'words))

(defconst franca-idl-font-lock-keywords
  (list
   '("\\<\\(PSM\\|a\\(?:rray\\|ttribute\\)\\|broadcast\\|c\\(?:all\\|ontract\\)\\|e\\(?:numeration\\|rror\\|xtends\\)\\|f\\(?:ireAndForget\\|rom\\)\\|i\\(?:mport\\|n\\(?:itial\\|terface\\)\\|[ns]\\)\\|m\\(?:a\\(?:jor\\|p\\)\\|ethod\\|inor\\|odel\\)\\|noSubscriptions\\|o\\(?:ut\\|[fn]\\)\\|package\\|readonly\\|s\\(?:elective\\|ignal\\|t\\(?:ate\\|ruct\\)\\)\\|t\\(?:o\\|ype\\(?:Collection\\|def\\)\\)\\|\\(?:un\\|vers\\)ion\\)\\>"
 . font-lock-keyword-face) ; builtin keywords
   '("\\<\\(?:B\\(?:oolean\\|yteBuffer\\)\\|Double\\|Float\\|Int\\(?:16\\|32\\|64\\|8\\)\\|String\\|UInt\\(?:16\\|32\\|64\\|8\\)\\)\\>" ; primitive types
 . font-lock-type-face))
  "Keyword highlighting for Franca IDL mode.")

;; FIXME: reposition closing brace when it get entered
(defun franca-idl-indent-line ()
  "Indent current line as Franca IDL code."
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "[^}]*}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "[^}]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "[^{]*{")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

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
(defun franca-idl-mode()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Franca-IDL"
        major-mode 'franca-idl-mode)
  (set-syntax-table franca-idl-syntax-table)
  (set (make-local-variable 'syntax-propertize-function) franca-idl-syntax-propertize-function)
  (use-local-map franca-idl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(franca-idl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'franca-idl-indent-line)
  (run-hooks 'franca-idl-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fidl\\'" . franca-idl-mode))

(provide 'franca-idl-mode)
