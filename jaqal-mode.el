;; Jaqal syntax highlighting and indentation major mode

(require 'cl-lib)
(require 'seq)

(defvar jaqal-mode-version "1.1.0")

(defvar jaqal-indent-width 2)

;; Set to automatically load for all .jaqal, .jql, and .jqm files
(let ((mode-list-entry (cons (regexp-opt '(".jaqal" ".jql" ".jqm")) 'jaqal-mode)))
  (add-to-list 'auto-mode-alist mode-list-entry)
  )

(defconst jaqal-font-lock-keywords-1
  (let ((header-keywords '("register" "map" "let" "from"))
	(other-keywords '("loop" "macro" "usepulses")))
    (list
     ;; Header keywords must come at the beginning of a line
     (cons (regexp-opt header-keywords "^\\(") font-lock-keyword-face)
     ;; These keywords need not be at the beginning of a line
     (cons (regexp-opt other-keywords) font-lock-keyword-face)
     ;; Make header keywords noticeable when inside a block.
     (cons (regexp-opt header-keywords) font-lock-warning-face)
     '("\\<-?[0-9]+\\.?[0-9]*\\(\\(e\\|E\\)-?[0-9]+\\)?\\>" . font-lock-constant-face)
     )))

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

(defun jaqal-parse-gatepulse ()
  "Return the gatepulse filename and class from the usepulses statement in this file"
  (save-mark-and-excursion
   (goto-char (point-min))
   (when (re-search-forward "^[[:blank:]]*from[[:blank:]]\\([[:alnum:]]+\\)\\.\\([[:alnum:]]+\\)[[:blank:]]+usepulses[[:blank:]]+\\*" nil t)
     (list (concat (match-string-no-properties 1) ".py") (match-string-no-properties 2)))))

(defun jaqal-find-gatepulse-file (filename &optional base-directory)
  "Find a gatepulse file starting with the given directory"
  (when filename
    (let* ((base-directory (or base-directory default-directory))
	   ;; add $ to the regexp to avoid things like file.py~
	   (all-files (directory-files-recursively base-directory (concat (regexp-quote filename) "$"))))
      (when all-files
	(car (last all-files))))))

(defun jaqal-goto-gate-definition ()
  "Find the definition for the gate at the point and go to it"
  (interactive)
  (let* ((identifier (jaqal-identifier-at-point))
	 (macros (jaqal-find-all-nocomment (concat "macro[[:blank:]]+" identifier))))
    (if (and identifier macros)
	(progn
	  (push-mark nil t)
	  (goto-char (car macros)))
      (let ((gatepulse-list (jaqal-parse-gatepulse)))
	(when gatepulse-list
	  (let ((gatepulse-file (jaqal-find-gatepulse-file (elt gatepulse-list 0)))
		(gatepulse-class (elt gatepulse-list 1)))
	    (when gatepulse-file
	      (when (find-file-other-window gatepulse-file)
		(when identifier
		  (jaqal-search-gate gatepulse-class identifier))))))))))

(defun jaqal-identifier-at-point ()
  "Return the identifier surrounding the point or nil"
  (save-mark-and-excursion
   (let ((ident-regexp "[[:alnum:]_]"))
     (skip-chars-backward "[[:alnum:]_]")
     (when (looking-at "[[:alpha:]_][[:alnum:]_]*")
       (match-string-no-properties 0)))))

(defun jaqal-search-gate (class identifier)
  "Find the given gate in the current buffer"
  (let ((gate-name (concat "gate_" identifier)))
    ;; We have to go a bit out of our way to not change the point and
    ;; mark if the gate does not exist.
    (let ((pos (save-mark-and-excursion
		(goto-char (point-min))
		(let ((search-start (re-search-forward (concat "^class[[:blank:]]+" class) nil t)))
		  (when search-start
		    (let ((search-end (or (re-search-forward "^class" nil t) (point-max))))
		      (car (jaqal-find-all-nocomment gate-name search-start search-end))))))))
      (when pos
	(goto-char pos)))))

(defun jaqal-find-all-nocomment (regexp &optional start end)
  "Find all instances of this regular expression that do not contain comments. Last element is head of the list."
  (setf start (or start (point-min)) end (or end (point-max)))
  (save-mark-and-excursion
   (goto-char start)
   (let ((ret nil))
     (while (re-search-forward regexp end t)
       (let ((match (match-string 0)))
	 (when (not (text-property-any 0 (length match) 'face 'font-lock-comment-face match))
	   (push (match-beginning 0) ret))))
     ret)))

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

(defvar jaqal-keymap (make-sparse-keymap))
(define-key jaqal-keymap (kbd "C-c C-d") #'jaqal-goto-gate-definition)

(defun jaqal-mode ()
  "Major mode for editing Jaqal quantum assembly files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(jaqal-font-lock-keywords))
  (set-syntax-table jaqal-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'jaqal-indent-line)
  (use-local-map jaqal-keymap)

  (setq indent-tabs-mode nil)
  (setq comment-start "//")
  (setq comment-padding " ")

  (setq major-mode 'jaqal-mode)
  (setq mode-name "Jaqal")
  )

(provide 'jaqal-mode)
