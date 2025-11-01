;;; gx-subrx.el --- Emacs Lisp Subroutines Xtra -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(require 'rx)



;;; Helper Functions

;;;###autoload
(defun gx/run-in-background (command)
  "Run COMMAND with arguments in background provided last argument is '&'."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process
           `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;;###autoload
(defun gx/run-command-with-output (command &optional filter message)
  "Run COMMAND providing output, optionally formatted with FILTER and MESSAGE."
  (unless (stringp command)
    (error "Command provided must be a string: %s" command))
  (unless filter (setq filter ""))
  (if message (setq message (concat message " ")) (setq message ""))
  (let ((output (shell-command-to-string command))
        (regex filter)
        (result ""))
    (when (string-match regex output)
      (setq result (match-string 0 output)))
    (message (concat message "%s") result)))

;; Facile passing of lists to `set-face-attribute', use only in theme setting.
;;;###autoload
(defun gx/set-face-attribute (face spec)
  "Set attributes FACE from SPEC.
FACE is expected to be a symbol with the same faces
as accepted by `set-face-attribute'.
SPEC is expected to be a plist with the same key names
as accepted by `set-face-attribute'.
FRAME is always set to nil"
  (when (and face spec)
    (apply 'set-face-attribute face nil spec)))

;;;###autoload
(defun list->add-to-list (list list-var)
  "Add LIST of items to LIST-VAR via`add-to-list'."
  (dolist (item list)
    (add-to-list list-var item)))

;;;###autoload
(defun gx/ensure-directory-exists (directory)
  "Ensure DIRECTORY exists by creating it if it doesn't."
  (let ((target-directory (expand-file-name directory)))
    (unless (file-directory-p target-directory)
      ;; The 't' argument creates parent directories if they don't exist.
      (make-directory target-directory t))))


;;; Emacs Lisp Syntax Extensions (aka Macros)

;;;###autoload
(defmacro gx/use-modules (&rest modules)
  "Conveniency macro that requires multiple MODULES."
  (declare (debug setq))
  (unless (symbolp (car modules))
    (error "Attemping to require a non-symbol: %s" (car modules)))
  (let ((expr nil))
    (while modules
      (push `(require ',(car modules)) expr)
      (setq modules (cdr modules)))
    (macroexp-progn (nreverse expr))))


;;;###autoload
(defmacro gx/ignore-messages (&rest body)
  "Ignore messages for BODY of called functions."
  (declare (indent 0))
  `(let ((inhibit-message t)
         (message-log-max nil))
     (progn ,@body)))



;;; Customize/Enhance setopt --> gx/setopts

;;;###autoload
(defmacro gx/setopts (&rest pairs)
  "Set VARIABLE/VALUE/[COMMENT] PAIRS, and return the final VALUE.
This is like `setq', but is meant for user options instead of
plain variables.  This means that `setopts' will execute any
`custom-set' form associated with VARIABLE.

Note that `setopts' will emit a warning if the type of a VALUE
does not match the type of the corresponding VARIABLE as
declared by `defcustom'.  (VARIABLE will be assigned the value
even if it doesn't match the type.)

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  ;; (unless (evenp (length pairs))
  ;;   (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      (cond ((stringp (caddr pairs))
             (push `(gx/setopts--set ',(car pairs) ,(cadr pairs) ,(caddr pairs))
                   expr)
             (setq pairs (cdddr pairs)))
            (t ;; defaults to what setopt does...
             (push `(gx/setopts--set ',(car pairs) ,(cadr pairs)) expr)
             (setq pairs (cddr pairs)))))
    (macroexp-progn (nreverse expr))))

;;;###autoload
(defun gx/setopts--set (variable value &optional comment)
  "Set VALUE and optionally COMMENT to VARIABLE using `custom-set'."
  (custom-load-symbol variable)
  ;; Check that the type is correct.
  (unless comment (setq comment ""))
  (when-let* ((type (get variable 'custom-type)))
    (unless (widget-apply (widget-convert type) :match value)
      (warn "Value `%S' for variable `%s' does not match its type \"%s\""
            value variable type)))
  (put variable 'custom-check-value (list value))
  (funcall (or (get variable 'custom-set) #'set-default) variable value)
  (cond ((string= comment "")
 	 (put variable 'variable-comment nil)
 	 (put variable 'customized-variable-comment nil))
 	(comment
 	 (put variable 'variable-comment comment)
 	 (put variable 'customized-variable-comment comment))))



;;; Hygenic Hooks Syntax

;; Keep Hook Functions & Hooks Hygenic...
;;;###autoload
(defun gx/flatten-list-one-level (list)
  "Flatten LIST by one level."
  (mapcan #'identity list))

;;;###autoload
(defmacro gx->defhook (symbol doc body &rest pairs)
  "Always create a well-defined hook function using DOC and BODY for SYMBOL.
Provide hook parameters from PAIRS of form :KEYWORD VALUE.

The following keywords are meaninful:

:hook  VALUE should be a variable type designating the hook which function named
       SYMBOL should be associated with.  VALUE may be a single hook, or a list
       of hooks.
:depth VALUE should conform `add-hook' spec for optional values.
:local VALUE should conform `add-hook' spec for optional values.
:args  VALUE should be a list of args, i.e. (arg1 arg2 ...) or arg (singular)
:defer VALUE should be an integer type designating the time in seconds to wait
       after hook has been called before running body of function named SYMBOL.
:disable? VALUE should be either nil (default) or t
:tbd   tbd...

\(fn SYMBOL [DOCSTRING] BODY KEYWORDS)"
  (declare (doc-string 2) (debug (name body)) (indent defun))
  (let ((disabled nil) (hooks nil) (depth 0) (local nil) (args '())
        (time nil) (exps '()))

    (while pairs
      (let ((keyword (pop pairs)))
        (unless (symbolp keyword)
          (error "Junk in pairs %S" pairs))
        (unless pairs
          (error "Keyword %s is missing an argument" keyword))
        (let ((value (pop pairs)))
          (pcase keyword
            (:hook (setq hooks (flatten-list value)))
            (:depth (setq depth value))
            (:local (setq local value))
            (:args (setq args (flatten-list value)))
            (:disable? (setq disabled value))
            (:defer (setq time value))))))

    (unless disabled
      (if time (setq body `((run-at-time ,time nil (lambda ,args ,@body)))))
      (if (and doc (>= (length doc) 1)) (push `(defun ,symbol ,args ,doc ,@body) exps)
        (push `(defun ,symbol ,args ,@body) exps))
      (while hooks
        (let (hook)
          (setq hook (pop hooks))
          (push `(add-hook ',hook #',symbol ,depth ,local) exps)))
      `(progn . ,(nreverse exps)))))

;;; Example usage
;; (gx->defhook my-hook-func
;;   "Hook function for testings"

;;   (;;function body
;;    (message "I am here!!! %s %s %s" first second third))

;;   :hook (first-hook second-hook third-hook)
;;   :depth 'append
;;   :local 'local
;;   :args (first second third)
;;   :defer 3)





(provide 'gx-subrx)
;;; gx-subrx.el ends here
