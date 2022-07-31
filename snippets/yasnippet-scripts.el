(defun my-yas-try-expanding-auto-snippets ()
    (when yas-minor-mode
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))

(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

; file_name: either '/kldb.tex' or '/idxdb.tex'
; cmd_path: either '/home/jose/.local/scripts/add-knowledge -p ' or
; '/home/jose/.local/scripts/add-glossary -p '
; prefix_entry: either ' -e \'index=' or ' -e \'newglossaryentry{'
; prefix_output: either "\\intro{" or "\\glsadd{"

(defun yasnippet/aux_add_cmd (file_name cmd_path prefix_entry prefix_output input)
  (let* ((curr_dir default-directory)
	 (curr_file (concat curr_dir file_name))
	 (curr_file_path (replace-regexp-in-string " " "\\\\ " curr_file))
	 (add_cmd (concat cmd_path curr_file_path " "))
	 (_args (split-string input ","))
	 (first_arg (car _args))
	 (args (mapconcat (lambda (x) (concat "\'" x "\'")) _args " "))
	 (command
	  (concat "if ! grep " curr_file prefix_entry first_arg "}\'; then "
		  add_cmd args "; fi")))
    (progn
      (if (not (file-exists-p curr_file)) (shell-command (concat "touch " curr_file_path)))
      (shell-command command)
      (concat prefix_output first_arg "}"))))

(defun yasnippet/input (input)
  (let* ((args (split-string input ","))
	 (num_args (length args)))
    (cond ((equal num_args 3)
	   (let ((name (nth 0 args))
		 (title_name (nth 1 args))
		 (latex_header (nth 2 args)))
	     (yasnippet/input_aux name title_name latex_header 3)))
	  ((equal num_args 2)
	   (let ((name (nth 0 args))
		 (title_name (nth 1 args)))
	     (yasnippet/input_aux name title_name "section" 2)))
	  ((equal num_args 1)
	   (let ((name (nth 0 args)))
	     (yasnippet/input_aux name "" "" 1)))
	  (t (yasnippet/input_aux "_new_file_" "new_title" "" num_args)))))

(defun yasnippet/input_aux (name title_name latex_header num_args)
  (let* ((file_name (concat name ".tex")))
    (shell-command (concat "touch " file_name))
    (if (and (< 1 num_args) (< num_args 4))
	(shell-command (concat "echo \\\\\'" latex_header "{" title_name "}\' > " file_name)))
    (concat "\\input\{" name "}")))

(defun yasnippet/repeat (x y)
  (let ((x_num (if (stringp x) (string-to-number x) x)))
    (let (value) (while (> x_num 0)
		   (setq value (concat value y))
		   (setq x_num (- x_num 1))) value)))

(defun yasnippet/find-char-position (x y n)
  (if (null x) (- 1)
    (if (equal (car x) y)
	n
      (yasnippet/find-char-position (cdr x) y (+ 1 n)))))

(defun yasnippet/parent-file ()
  (let* ((file_name (substring (buffer-name) 0 -4))
	 (grep_command (concat "grep -nr input{" file_name "}"))
	 (grep_output (shell-command-to-string grep_command))
	 (position (yasnippet/find-char-position (string-to-list grep_output) 58 0)))
    (if (equal position (- 1)) "" (substring grep_output 0 position))))

(defun yasnippet/goto-parent-file ()
  (let ((file (yasnippet/parent-file)))
    (if (not (equal file "")) (find-file file))))

(defun yasnippet/count-delims-table (x n)
  (if (null x) n
    (let ((curr (car x)))
      (if (or (equal curr 99) (equal curr 108) (equal curr 114))
	  (yasnippet/count-delims-table (cdr x) (+ 1 n))
	(yasnippet/count-delims-table (cdr x) n)))))

;; function to return first name of email recipients
;; used by yasnippet
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html

(defun yasnippet/mu4e-get-names ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))
