(defvar autoc-marker-prefix "autoc"
  "The base autoc marker prefix")

(defvar autoc-marker-block ":"
  "Operate on block (requires block end marker)")

(defvar autoc-marker-end "#"
  "Block end marker")

(defvar autoc-marker-command "!"
  "Autoc command (single line)")

(require 's)
(require 'thing-at-point)

(defun autoc-word-at-point-is-marker-p (&optional suffix)
  "Return t if the word at point is an autoc marker ending in suffix"
  (s-starts-with-p (concat autoc-marker-prefix suffix)
                   (buffer-substring-no-properties (point)
                                                   (line-end-position))
                   'ignore-case))

(defun autoc-previous-marker (&optional suffix)
  "Return point at the start of the previous autoc marker. If
   suffix is provided, the marker must have the given suffix"
  (save-excursion
    (when (search-backward autoc-marker-prefix nil t)
      (if (autoc-word-at-point-is-marker-p suffix)
          (point)
        (progn
          (forward-line -1)
          (end-of-line)
          (autoc-previous-marker suffix))))))

(defun autoc-next-marker (&optional suffix)
  "Return point at the start of the next autoc marker"
  (save-excursion
    (when (search-forward autoc-marker-prefix nil t)
      ;; search-forward puts point at end of marker, so move left:
      (left-char (length autoc-marker-prefix))
      (if (autoc-word-at-point-is-marker-p suffix)
          (point)
        (progn
          (forward-line 1)
          (autoc-next-marker suffix))))))

(defun autoc-get-marker-args (line)
  "Get all the arguments for the marker in the given line

       autoc:lines foo bar   => '(lines foo bar)
       autoc: funcall alice  => '(funcall alice)
       autoc! blah           => '(blah)"
  (string-match (concat autoc-marker-prefix ".\\(.*\\)") line)
  (read (concat "(" (s-trim (match-string 1 line)) ")")))

(defun autoc-block-start ()
  "Return point at the start of the current block

              e.g. with point before 'block':

              autoc:something
              inside |block
              autoc#

              point returned:

              autoc:something
              |inside block
              autoc:end"
  (save-excursion
    (end-of-line)
    (let ((pos (autoc-previous-marker autoc-marker-block)))
      (when pos
        (goto-char pos)
        (next-line)
        (line-beginning-position)))))

(defun autoc-block-end (start)
  "Return point at the end of the block starting at `start'

              e.g. with point before 'block':

              autoc:something
              inside |block
              autoc:end

              point returned:

              autoc:something
              inside block
              |autoc:end"
  (save-excursion
    (goto-char start)
    (let ((pos (autoc-next-marker autoc-marker-end)))
      (when pos
        (goto-char pos)
        (line-beginning-position)))))

(defun autoc-in-block-p ()
  "Return t if point is on any line in a block, including the start and end marker lines"
  (interactive)
  (let* ((start (autoc-block-start))
         (end (autoc-block-end start))
         (pos (line-number-at-pos)))
    (and start
         end
         (>= pos (- (line-number-at-pos start) 1))
         (<= pos (line-number-at-pos end)))))

(defmacro autoc-when-in-block (&rest body)
  "Evaluate `body' if currently in a block, alerting the user otherwise"
  `(if (autoc-in-block-p)
       (progn
         ,@body)
     (message "autoc: not in a block")))

(defun autoc-kill-block ()
  "Delete all content in the current block"
  (interactive)
  (let* ((start (autoc-block-start))
         (end (autoc-block-end start)))
    (when (> (count-lines start end) 0)
      (kill-region start end))))

(defun autoc-block-contents ()
  "Get the contents of the current block"
  (let* ((start (autoc-block-start))
         (end (autoc-block-end start)))
    (s-trim (buffer-substring-no-properties start end))))

(defun autoc-block-get-marker-line ()
  "Get the full starting marker line for the current block"
  (save-excursion
    (goto-char (autoc-block-start))
    (previous-line)
    (s-trim (thing-at-point 'line t))))

(defun autoc-end-of-current-block ()
  "Go to the end of the block under point if it exists"
  (interactive)
  (autoc-when-in-block
   (goto-char (autoc-block-end (autoc-block-start)))))

(defvar autoc-operations-plist nil
  "List of operations and their functions")

(defun autoc-get-operation-fn (symbol)
  "Return the function implementing the operation"
  (plist-get autoc-operations-plist symbol))

(defun autoc-has-operation-fn-p (symbol)
  "Return t if the given operation exists"
  (plist-member autoc-operations-plist symbol))

(defun autoc-add-operation (symbol function)
  "Add a operation. If it already exists, it is replaced"
  (setq autoc-operations-plist
        (plist-put autoc-operations-plist symbol function)))

(defun autoc-run-operation (operation args)
  "Run the `operation' with `args' if possible"
  ;; TODO if len op is 1, lookup in aliases
  (if (autoc-has-operation-fn-p operation)
      (apply (autoc-get-operation-fn operation) args)
    (message (format "Unknown operation: ~A" operation))))

;; TODO defcustom
(defvar autoc-aliases-alist
  '(("=" . block)
    ("\\" . funcall)
    (">" . format-lines)))

(defun autoc-run-line-operation (line)
  "Run the operation for the given marker line"
  (let* ((marker-args (autoc-get-marker-args line))
         (operation (first marker-args))
         (args (rest marker-args)))
    (autoc-run-operation operation args)))

(defun autoc-run-block-operation ()
  "Run the autoc operation for the current block"
  (interactive)
  (autoc-when-in-block
   (let ((marker-line (autoc-block-get-marker-line)))
     (autoc-run-line-operation marker-line))))

(defmacro def-autoc-op (name lambda-list &rest body)
  "Define an operation with the given name and argument list"
  `(autoc-add-operation ',name (lambda ,lambda-list ,@body)))

(defmacro autoc-replace-block (&rest body)
  "Kill the current block and then execute body at the start of the block"
  `(autoc-when-in-block
    (autoc-kill-block)
    (goto-char (autoc-block-start))
    ,@body))

(make-variable-buffer-local
 (defvar autoc-source-text-alist nil
   "alist of source text blocks - key is block name"))

(def-source defun (block-text lambda-list)
  "Define an arbitrary function"
  (lambda
    ))

(def-autoc-op block (name)
  (autoc-when-in-block
   (set (make-local-variable name) (autoc-block-contents))))

(def-autoc-op lines (name &optional fn)
  (autoc-when-in-block
   (let* ((content (autoc-block-contents))
          (modifier (or fn #'identity))
          (lines (map 'list modifier (s-split "\n" content))))
     (set (make-local-variable name) lines))))

(def-autoc-op format-lines (lines fmt)
  (autoc-replace-block
   (dolist (l (symbol-value lines))
     (insert (format fmt l))
     (newline-and-indent))))

;;;###autoload
(define-minor-mode autoc-mode
  "Automatic embedded code generation"
  :lighter " autoc"
  :keymap (make-sparse-keymap)
  (progn
    (autoc-load-sources)))

(let ((map autoc-mode-map))
  (define-key map (kbd "C-c e e") 'autoc-run-block-operation)
  map)
