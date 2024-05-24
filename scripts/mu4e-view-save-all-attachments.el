;;; mu4e-view-save-all-attachments.el -- Save all attachments from view mode.
;;; Stephen J Eglen 2021


;; I've created this based on the work of Phil Jackson that required
;; an older version of mu4e.  This version requires the GNUS article
;; code for reading mu4e messages.
;; https://gist.github.com/philjackson/aecfab1706f05079aec7000e328fd183

;; Suggested keybinding
;;  mnemnonic: > is to redirect the files to output everything.

(define-key mu4e-view-mode-map (kbd ">") 'mu4e-view-save-all-attachments)
;(define-key evil-normal-state-map (kbd ">") 'mu4e-view-save-all-attachments)

(defvar bulk-saved-attachments-dir mu4e-attachment-dir)

(defun cleanse-subject (sub)
  (replace-regexp-in-string
   "[^A-Z0-9]+"
   "-"
   (downcase sub)))

(defun mu4e-view-save-all-attachments  (&optional arg)
  "Save mime parts from current mu4e gnus view buffer.

    When helm-mode is enabled provide completion on attachments and
    possibility to mark candidates to save, otherwise completion on
    attachments is done with `completing-read-multiple', in this case
    use \",\" to separate candidate, completion is provided after
    each \",\".

    Note, currently this does not work well with file names
    containing commas."
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((msg (mu4e-message-at-point))
         (id (mu4e-message-field msg :subject))
         (attachdir (concat bulk-saved-attachments-dir "/" id))
	 (parts (mu4e-view-mime-parts))
	 (candidates  (seq-map
                         (lambda (fpart)
                           (cons ;; (filename . annotation)
                            (plist-get fpart :filename)
                            fpart))
                         (seq-filter
                          (lambda (part) (plist-get part :attachment-like))
                          parts)))
	 (handles (or candidates
                         (mu4e-warn "No attachments for this message")))
	 (files (seq-map (lambda (h) (car h)) handles))
         dir)
    (mkdir attachdir t)
    (seq-do (lambda (fname)
              (let* ((dir (if arg (read-directory-name "Save to directory: ") attachdir))
		     (part (cdr (assoc fname candidates)))
                     (path (mu4e--uniqify-file-name
                            (mu4e-join-paths
                             (or dir (plist-get part :target-dir))
                             (plist-get part :filename)))))
                (mm-save-part-to-file (plist-get part :handle) path)))
            files)))

(defun sje-next-free (file)
  "Return name of next unique 'free' FILE.
If /tmp/foo.txt and /tmp/foo-1.txt exist, when this is called
with /tmp/foo.txt, return /tmp/foo-2.txt.  See
`sje-test-next-free' for a test case.  This is not very efficient
if there are a large number of files already in the directory
with the same base name, as it simply starts searching from 1
each time until it finds a gap.  An alternative might be to do a
wildcard search for all the filenames, extract the highest number
and then increment it."
  ;; base case is easy; does file exist already?
  (if (not  (file-exists-p file))
      file
    ;; othwerwise need to iterate through f-1.pdf
    ;; f-2.pdf, f-3.pdf ... until we no longer find a file.
    (let ((prefix (file-name-sans-extension file))
	  (suffix (file-name-extension file))
	  (looking t)
	  (n 0)
	  (f)
	  )
      (while looking
	(setq n (1+ n))
	(setq f (concat prefix "-" (number-to-string n) "." suffix))
	(setq looking (file-exists-p f)))
      f
      )))


(defun sje-test-next-free ()
  (let (f)
    (dotimes (i 100)
      (setq f (sje-next-free "/tmp/rabbit.txt"))
      (write-region "hello" nil f)
      )))
;; (sje-test-next-free)
