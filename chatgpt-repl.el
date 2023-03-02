;;; chatgpt-repl.el ---                              -*- lexical-binding: t; -*-
;; Keywords: tools

(require 'dash)

(defvar chatgpt-bin (executable-find "chatgpt"))
(defvar chatgpt-buffer-name "*chatgpt-repl*")
(defvar chatgpt-err-buffer-name "*chatgpt-repl-err*")

(defun chatgpt-repl-insert-prompt ()
  (insert "\n> ")
  (comint-set-process-mark))

(defun chatgpt-bin-eval (input buffer)
  (make-process :name "chatgpt-repl-bin"
		:command (list chatgpt-bin input)
		:stderr (get-buffer-create chatgpt-err-buffer-name)
		:filter (lambda (proc string)
			  (with-current-buffer buffer
			    (insert string)))
		:sentinel (lambda (proc event)
			    (when (not (process-live-p proc))
			      (with-current-buffer buffer
				(chatgpt-repl-insert-prompt)))))
  nil)

(defun chatgpt-repl-rep (input buffer)
  (chatgpt-bin-eval input buffer))

(defun chatgpt-repl-send-input ()
  (interactive)
  (let ((input (buffer-substring
		(process-mark (get-buffer-process (current-buffer)))
		(point-max))))
    (if (string-empty-p (string-trim input))
	(chatgpt-repl-insert-prompt)
      (progn
	(comint-send-input)
	(chatgpt-repl-rep input (current-buffer))))))

(defun chatgpt-repl-move-beginning-of-line ()
  (interactive)
  (let ((pos (comint-line-beginning-position)))
    (if (= (point) pos)
        (call-interactively #'back-to-indentation)
      (goto-char pos))))

(defvar chatgpt-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chatgpt-repl-send-input)
    (define-key map (kbd "C-a") #'chatgpt-repl-move-beginning-of-line)
    map))

(define-derived-mode chatgpt-repl-mode comint-mode "chatgpt-repl"
  ""
  (setq comint-prompt-regexp "^> ")
  (setq comint-input-sender (lambda (proc s) nil))
  (setq comint-process-echoes nil)
  (setq comint-use-prompt-regexp t)

  (unless (comint-check-proc (current-buffer))
    (let ((process (start-process "chatgpt-repl" (current-buffer) "hexl")))
      (set-process-query-on-exit-flag process nil)
      (insert "ChatGPT-REPL\n")
      (insert "make sure (get-env \"OPENAI_API_KEY\") and (executable-find \"chatgpt\") exist,\n")
      (insert "install chatgpt bin: npm install chatgpt -g\n")
      (insert "set env: M-x set-env")
      (chatgpt-repl-insert-prompt))))

;;;###autoload
(defun chatgpt-repl ()
  (interactive)
  (let ((buffer (get-buffer-create chatgpt-buffer-name)))
    (pop-to-buffer buffer)
    (chatgpt-repl-mode)))

;;;###autoload
(defun chatgpt-repl-send (string)
  (interactive "s")
  (let ((buffer (get-buffer chatgpt-buffer-name)))
    (when (not buffer)
      (call-interactively 'chatgpt-repl))
    (with-current-buffer chatgpt-buffer-name
      (goto-char (point-max))
      (insert string)
      (call-interactively 'chatgpt-repl-send-input))
    (pop-to-buffer chatgpt-buffer-name)))

;;;###autoload
(defun chatgpt-translate-region ()
  (interactive)
  (when (not (region-active-p))
    (user-error "no region active"))
  (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
    (chatgpt-repl-send (format "translate into english or chinese: %s" s))))

(provide 'chatgpt-repl)
