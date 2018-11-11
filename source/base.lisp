;;; base.lisp --- main entry point into Next

(in-package :next)


(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit))

(defun parse-cli-args ()
  "Parse command line arguments."
  (opts:define-opts
    (:name :help
           :description "Print this help and exit."
           :short #\h
           :long "help")
    (:name :verbose
           :short #\v
           :long "verbose"
           :description "Print debugging information to stdout."))

  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(defun start ()
  (map nil 'funcall *deferred-variables*)
  (ensure-directories-exist (xdg-data-home))
  (initialize-default-key-bindings)
  ;; load the user configuration if it exists
  (load *init-file-path* :if-does-not-exist nil)
  (initialize-bookmark-db)
  (initialize-history-db)
  ;; create the interface object
  (setf *interface* (make-instance 'remote-interface))
  (start-interface *interface*)
  ;; initialize default state
  (setf *minibuffer* (make-instance 'minibuffer))
  (make-window))

(defun initialize-default-key-bindings ()
  (define-key *global-map* (kbd "C-x C-c") '(lambda () (kill-interface *interface*)))
  (define-key *global-map* (kbd "C-x b") 'switch-buffer)
  (define-key *global-map* (kbd "C-x k") 'delete-buffer)
  (define-key *global-map* (kbd "M-l") 'set-url-new-buffer)
  (define-key *global-map* (kbd "S-b k") 'bookmark-delete)
  (define-key *global-map* (kbd "C-t") 'make-visible-new-buffer)
  (define-key *global-map* (kbd "S-b u") 'bookmark-url)
  (define-key *global-map* (kbd "C-x w") 'delete-active-buffer)
  (define-key *global-map* (kbd "S-h v") 'variable-inspect)
  (define-key *global-map* (kbd "S-h c") 'command-inspect)
  (define-key *global-map* (kbd "C-o") 'load-file)
  (define-key *global-map* (kbd "S-h s") 'start-swank)
  (define-key *global-map* (kbd "C-y") '(lambda () (paste *interface*)))
  (define-key *global-map* (kbd "C-w") '(lambda () (cut *interface*)))
  (define-key *global-map* (kbd "M-w") '(lambda () (copy *interface*)))
  (define-key *global-map* (kbd "C-x 5 3") #'(lambda () (print (window-active *interface*))))
  (define-key *global-map* (kbd "C-x 5 2") 'make-window)
  (define-key *global-map* (kbd "C-x 5 0") 'delete-window)
  (define-key *document-mode-map* (kbd "M-f") 'history-forwards-query)
  (define-key *document-mode-map* (kbd "M-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-g") 'go-anchor)
  (define-key *document-mode-map* (kbd "M-g") 'go-anchor-new-buffer)
  (define-key *document-mode-map* (kbd "S-g") 'go-anchor-new-buffer-focus)
  (define-key *document-mode-map* (kbd "C-f") 'history-forwards)
  (define-key *document-mode-map* (kbd "C-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-p") 'scroll-up)
  (define-key *document-mode-map* (kbd "C-n") 'scroll-down)
  (define-key *document-mode-map* (kbd "C-x C-=") 'zoom-in-page)
  (define-key *document-mode-map* (kbd "C-x C-HYPHEN") 'zoom-out-page)
  (define-key *document-mode-map* (kbd "C-x C-0") 'unzoom-page)
  (define-key *document-mode-map* (kbd "C-l") 'set-url-current-buffer)
  (define-key *document-mode-map* (kbd "S-b o") 'set-url-from-bookmark)
  (define-key *document-mode-map* (kbd "S-b s") 'bookmark-current-page)
  (define-key *document-mode-map* (kbd "S-b g") 'bookmark-anchor)
  (define-key *document-mode-map* (kbd "C-[") 'switch-buffer-previous)
  (define-key *document-mode-map* (kbd "C-]") 'switch-buffer-next)
  (define-key *document-mode-map* (kbd "S-s s") 'add-search-boxes)
  (define-key *document-mode-map* (kbd "S-s n") 'next-search-hint)
  (define-key *document-mode-map* (kbd "S-s p") 'previous-search-hint)
  (define-key *document-mode-map* (kbd "S-s k") 'remove-search-hints)
  (define-key *document-mode-map* (kbd "C-.") 'jump-to-heading)
  (define-key *document-mode-map* (kbd "M->") 'scroll-to-bottom)
  (define-key *document-mode-map* (kbd "M-<") 'scroll-to-top)
  ;;; define self-insert commands for minibuffer for every type of character
  (loop for code below 128 do
    (let ((char (code-char code)))
      (define-key *minibuffer-mode-map* (kbd (string char))
        (lambda () (self-insert *minibuffer* (string char))))))
  (define-key *minibuffer-mode-map* (kbd "SPACE") #'(lambda () (self-insert *minibuffer* " ")))
  (define-key *minibuffer-mode-map* (kbd "C-f") #'(lambda () (cursor-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-b") #'(lambda () (cursor-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-d") #'(lambda () (delete-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "BACKSPACE") #'(lambda () (delete-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-a") #'(lambda () (cursor-beginning *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-e") #'(lambda () (cursor-end *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "RETURN") #'(lambda () (return-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-RETURN") #'(lambda () (return-immediate (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-g") #'(lambda () (cancel-input (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "ESCAPE") #'(lambda () (cancel-input (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-n") #'(lambda () (select-next (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-p") #'(lambda () (select-previous (mode *minibuffer*)))))
