;; Jaqal syntax highlighting and indentation major mode

(require 'cl-lib)
(require 'seq)

(defvar jaqal-mode-version "1.0.1")

(defvar jaqal-indent-width 2)

;; Set to automatically load for all .jaqal, .jql, and .jqm files
(let ((mode-list-entry (cons (regexp-opt '(".jaqal" ".jql" ".jqm")) 'jaqal-mode)))
  (add-to-list 'auto-mode-alist mode-list-entry)
  )

(defconst jaqal-font-lock-keywords-1
  (list
   ;; Header keywords must come at the beginning of a line
   (cons (regexp-opt '("register" "map" "let") "^\\(") font-lock-keyword-face)
   ;; These keywords can be inside a block
   (cons (regexp-opt '("loop" "macro")) font-lock-keyword-face)
   (cons (regexp-opt '("register" "map" "let")) font-lock-warning-face)
   '("\\<-?[0-9]+\\.?[0-9]*\\(\\(e\\|E\\)-?[0-9]+\\)?\\>" . font-lock-constant-face)
   ))

(defvar jaqal-font-lock-keywords jaqal-font-lock-keywords-1
  "Default highlighting expressions for Jaqal mode")

(defun jaqal-open-bracket-p (char)
  "Return whether the given character is an open bracket in Jaqal"
  (or (char-equal char ?{) (char-equal char ?<)))

(defun jaqal-close-bracket-p (char)
  "Return whether the given character is a close bracket in Jaqal"
  (or (char-equal char ?}) (char-equal char ?>)))

(defun jaqal-bracket-p (char)
  "Return whether the given character is a bracket in Jaqal"
  (or (jaqal-open-bracket-p char) (jaqal-close-bracket-p char)))

(defun jaqal-count-open-bracket-diff (str)
  "Count open brackets not closed in this string."
  (cl-flet
      ((bracket-acc
	(acc char)
	(let ((open-offset (if (jaqal-open-bracket-p char) 1 0))
	      (close-offset (if (jaqal-close-bracket-p char) -1 0)))
  	  ; Make sure that if we hit close brackets without open
  	  ; brackets we don't count them
	  (max 0 (+ acc open-offset close-offset)))))
    (seq-reduce #'bracket-acc str 0)))

(defun jaqal-count-close-bracket-diff (str)
  "Count close brackets not just opened in this string."
  (cl-flet
      ((bracket-acc
	(acc char)
	(let ((close-offset (if (jaqal-close-bracket-p char) 1 0))
	      (open-offset (if (jaqal-open-bracket-p char) -1 0)))
	  ; Make sure that open brackets at the end (since we
	  ; iterate in reverse) are not counted
	  (max 0 (+ acc close-offset open-offset)))))
    (seq-reduce #'bracket-acc (reverse str) 0)))

(defun jaqal-indent-line ()
  "Indent current line of Jaqal code"
  (interactive)
  (cl-flet
      ((get-curline () (buffer-substring-no-properties
			(line-beginning-position)
			(line-end-position)))
       (line-reference-p (curline)
			 (or (bobp)
			     (/= (seq-count 'jaqal-bracket-p curline) 0))))
  (save-mark-and-excursion
   (beginning-of-line)
   (if (bobp)
       (indent-line-to 0)
     (let* ((curline (get-curline))
	    ;; This isn't quite right either. Think about what the
	    ;; right thing to do here is.
	    (curbrackets (jaqal-count-close-bracket-diff curline))
	    (curindent (current-indentation))
	    (newindent (save-mark-and-excursion
			(next-line -1)
			(beginning-of-line)
			(while (not (line-reference-p (get-curline)))
			  (next-line -1)
			  (beginning-of-line))
			(let* ((prevline (get-curline))
			       (prevbrackets
				(jaqal-count-open-bracket-diff prevline))
			       (previndent (current-indentation)))
			  (max (+
				previndent
				(* (- prevbrackets curbrackets)
				   jaqal-indent-width))
			       0)))))
       (indent-line-to newindent)
       )
     )
   )
  )
  )

(defvar jaqal-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "w" st) ;; Otherwise " starts a quote
    (modify-syntax-entry ?\' "w" st) ;; Otherwise ' starts a quote
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")" st) ;; otherwise this is punctuation
    (modify-syntax-entry ?{ "(}" st)
    st)
  "Syntax table for Jaqal mode")

(defun jaqal-mode ()
  "Major mode for editing Jaqal quantum assembly files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(jaqal-font-lock-keywords))
  (set-syntax-table jaqal-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'jaqal-indent-line)

  (setq indent-tabs-mode nil)

  (setq major-mode 'jaqal-mode)
  (setq mode-name "Jaqal")
  )

(provide 'jaqal-mode)
