;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-mode
  (:use :common-lisp :nyxt)
  (:shadow #:focus-first-input-field)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Mode for web pages"))
(in-package :nyxt/web-mode)
(use-nyxt-package-nicknames)

;; TODO: Remove web-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode web-mode ()
  "Base mode for interacting with documents."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "web"
      scheme:cua
      (list
       "C-M-Z" 'nyxt/passthrough-mode:passthrough-mode
       "M-i" 'focus-first-input-field
       "C-c" 'copy
       "C-v" 'paste
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-hyphen" 'unzoom-page
       "C-0" 'reset-page-zoom
       "C-button4" 'zoom-page
       "C-button5" 'unzoom-page
       "C-M-c" 'open-inspector
       "C-." 'jump-to-heading
       "M-{" 'previous-heading
       "M-}" 'next-heading
       "end" 'maybe-scroll-to-bottom
       "home" 'maybe-scroll-to-top
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       "C-u C-o" 'edit-with-external-editor
       ;; Leave SPACE and arrow keys unbound so that the renderer decides whether to
       ;; navigate textboxes (arrows), insert or scroll (space).
       ;; keypad, gtk:
       "keypadleft" 'scroll-left
       "keypaddown" 'scroll-down
       "keypadup" 'scroll-up
       "keypadright" 'scroll-right
       "keypadend" 'scroll-to-bottom
       "keypadhome" 'scroll-to-top
       "keypadnext" 'scroll-page-down
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up)
      scheme:emacs
      (list
       "C-g" 'nothing              ; Emacs users may hit C-g out of habit.
       "C-y" 'paste
       "M-w" 'copy
       "C-/" 'undo
       "C-?" 'redo ; / shifted on QWERTY
       "C-w" 'cut
       "C-x h" 'select-all
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "C-x C-+" 'zoom-page
       "C-x C-=" 'zoom-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'unzoom-page
       "C-x C-0" 'reset-page-zoom
       "C-." 'jump-to-heading
       "M->" 'scroll-to-bottom
       "M-<" 'scroll-to-top
       "C-v" 'scroll-page-down
       "M-v" 'scroll-page-up
       "C-u C-x C-f" 'edit-with-external-editor)

      scheme:vi-normal
      (list
       "y y" 'copy
       "p" 'paste
       "d d" 'cut
       "u" 'undo
       "C-r" 'redo
       "+" 'zoom-page
       "hyphen" 'unzoom-page
       "0" 'reset-page-zoom
       "z i" 'zoom-page
       "z o" 'unzoom-page
       "z z" 'reset-page-zoom
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
       "{" 'previous-heading
       "}" 'next-heading
       "h" 'scroll-left
       "j" 'scroll-down
       "k" 'scroll-up
       "l" 'scroll-right
       "G" 'scroll-to-bottom
       "g g" 'scroll-to-top
       "C-f" 'scroll-page-down
       "C-b" 'scroll-page-up
       "space" 'scroll-page-down
       "s-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "pagedown" 'scroll-page-down)))))

(sera:export-always '%clicked-in-input?)
(defun %clicked-in-input? (&optional (buffer (current-buffer)))
  ;; We don't use define-parenscript because we need to control over which
  ;; buffer we query.
  (ffi-buffer-evaluate-javascript buffer
                                  (ps:ps
                                    (ps:chain document active-element
                                              tag-name))))

(sera:export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(defun call-non-input-command-or-forward (command &key (buffer (current-buffer))
                                                       (window (current-window)))
  (let ((response (%clicked-in-input?)))
    (if (input-tag-p response)
        (forward-to-renderer :window window :buffer buffer)
        (funcall command))))

(define-command maybe-scroll-to-bottom (&optional (buffer (current-buffer)))
  "Scroll to bottom if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-bottom :buffer buffer))

(define-command maybe-scroll-to-top (&optional (buffer (current-buffer)))
  "Scroll to top if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-top :buffer buffer))

(define-command go-next ()
  "Navigate to the next element according to the HTML 'rel' attribute."
  (peval (ps:chain (nyxt/ps:qsa document "rel=next") 0 (click))))

(define-command go-previous ()
  "Navigate to the previous element according to the HTML 'rel' attribute."
  (peval (ps:chain (nyxt/ps:qsa document "rel=prev") 0 (click))))

(define-command go-to-homepage ()
  "Navigate to the homepage."
  (let* ((url (url (current-buffer)))
         (authority (quri:uri-authority url))
         (scheme (quri:uri-scheme url)))
    (buffer-load (str:concat scheme "://" authority))))

(define-command go-up ()
  "Navigate to the upper level in the URL path hierarchy."
  (let* ((url (url (current-buffer)))
         (path (quri:uri-path url))
         (path-splited (str:split "/" path :omit-nulls t))
         (new-path-splited (butlast path-splited))
         (scheme (quri:uri-scheme url))
         (authority (quri:uri-authority url))
         (new-path (reduce #'(lambda (x e) (str:concat x e "/"))
                           new-path-splited
                           :initial-value "/")))
    (buffer-load (str:concat scheme "://" authority new-path))))


(define-command paste (&optional (buffer (current-buffer)))
  "Paste from clipboard into active element."
  (ffi-buffer-paste buffer))

(define-class ring-source (prompter:source)
  ((prompter:name "Clipboard ring")
   (ring :initarg :ring :accessor ring :initform nil)
   (prompter:constructor
    (lambda (source)
      (containers:container->list (ring source))))
   (prompter:actions
    (list (lambda-command paste* (ring-items)
            (%paste :input-text (first ring-items))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (prompt
   :prompt "Paste from ring"
   :sources (list (make-instance 'ring-source
                                 :ring (nyxt::clipboard-ring *browser*)))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (ffi-buffer-copy buffer))

(define-command copy-placeholder ()
  "Copy placeholder text to clipboard."
  (let ((current-value (peval (ps:@ document active-element placeholder))))
    (if (eq current-value :undefined)
        (echo "No active selected placeholder.")
        (progn (copy-to-clipboard current-value)
               (echo "Placeholder copied.")))))

(define-command cut (&optional (buffer (current-buffer)))
  "Cut the selected text in BUFFER."
  (ffi-buffer-cut buffer))

(define-command undo (&optional (buffer (current-buffer)))
  "Undo the last editing action."
  (ffi-buffer-undo buffer))

(define-command redo (&optional (buffer (current-buffer)))
  "Redo the last editing action."
  (ffi-buffer-redo buffer))

(define-command select-all (&optional (buffer (current-buffer)))
  "Select all the text in the text field."
  (ffi-buffer-select-all buffer))

(export-always 'element-focused)
(defgeneric element-focused (mode) ; TODO: Make hook instead?  Or use both, have the default method call hook.
  (:method ((mode t))
    nil)
  (:documentation "Method run when `focus-element' is called."))

(defmacro focus-element ((&optional (buffer '(current-buffer))) &body element-script)
  "Select the element pointed to by ELEMENT-SCRIPT.
ELEMENT-SCRIPT is a Parenscript script that is passed to `ps:ps'."
  (alex:with-gensyms (element)
    (alex:once-only (buffer)
      `(progn
         (ffi-buffer-evaluate-javascript ,buffer
                                         (ps:ps (let ((,element (progn ,@element-script)))
                                                  (ps:chain ,element (focus))
                                                  (ps:chain ,element (select)))))
         (dolist (mode (modes ,buffer))
           (element-focused mode))))))

(define-command focus-first-input-field (&key (type-blacklist '("hidden"
                                                                "checkbox"
                                                                "button")))
  "Move the focus to the first input field of `buffer'."
  ;; The list of input types can be found below.
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
  ;; TODO: The following results in 2 DOM traversal.  We should probably do the
  ;; whole thing in a single Parenscript instead.
  (pflet ((nth-input-type (i)
                          (let* ((input (ps:chain document
                                                  (get-elements-by-tag-name "INPUT")))
                                 (item (when input (ps:chain input (item (ps:lisp i))))))
                            (when item
                              (ps:chain item type)))))
    (let ((i (do ((i 0 (1+ i)))
                 ((notany
                   (lambda (type) (equalp (nth-input-type i) type))
                   type-blacklist)
                  i))))
      (focus-element ()
        (let* ((input (ps:chain document
                                (get-elements-by-tag-name "INPUT")))
               (item (when input (ps:chain input (item (ps:lisp i))))))
          (when item
            item))))))

(defmethod nyxt:on-signal-load-committed ((mode web-mode) url)
  (declare (ignore mode url))
  nil)

(defmethod nyxt:on-signal-load-finished ((mode web-mode) url)
  (reset-page-zoom :buffer (buffer mode)
                   :ratio (current-zoom-ratio (buffer mode)))
  url)

;; REVIEW: Shorten the name to e.g., `show-url-qr'? It's no longer current URL only.
(define-internal-page-command-global show-qrcode-of-current-url
    (&key (buffer-id (id (current-buffer)))
     (url (quri:render-uri (url (nyxt::buffers-get buffer-id)))))
    (buffer (format nil "*Buffer ~a (~a) QRcode*" buffer-id url) 'base-mode)
  "In a new buffer, show the QR code containing the URL for the current buffer."
  (let* ((stream (flexi-streams:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:p (:u url))
      (:p (:img :src (str:concat "data:image/png;base64,"
                                 (cl-base64:usb8-array-to-base64-string
                                  (flexi-streams:get-output-stream-sequence stream)))
                :alt url)))))

(define-internal-page-command-global view-source (&key (url (render-url (url (current-buffer)))))
  (source-buffer (format nil "*Source of ~a" url) 'base-mode)
  "View source of the URL (by default current page) in a separate buffer."
  (let ((buffer (or (find (quri:uri url) (buffer-list) :test #'quri:uri= :key #'url)
                    (make-background-buffer :url url))))
    (unwind-protect
         (spinneret:with-html-string
           (:pre (if (web-buffer-p buffer)
                     (plump:serialize (document-model buffer) nil)
                     (ffi-buffer-get-document buffer))))
      (when (background-buffer-p buffer)
        (ffi-buffer-delete buffer)))))

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (peval (ps:chain window (scroll-by 0 (- (ps:chain document document-element scroll-height))))))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (peval (ps:chain window (scroll-by 0 (ps:chain document document-element scroll-height)))))

(define-command scroll-down (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (peval (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command scroll-up (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (peval (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command scroll-left (&key (horizontal-scroll-distance
                                   (horizontal-scroll-distance (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (peval (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0))))

(define-command scroll-right (&key (horizontal-scroll-distance
                                    (horizontal-scroll-distance (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (peval (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0))))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (peval (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                          (ps:@ window inner-height))))))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (peval (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height)))))))

(defun ensure-zoom-ratio-range (zoom &optional (buffer (current-buffer)))
  (let* ((ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-command zoom-page (&key (buffer (current-buffer)))
  "Zoom in the current page."
  (ensure-zoom-ratio-range #'+ (current-buffer))
  (ffi-buffer-set-zoom-level buffer (current-zoom-ratio (current-buffer))))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (ensure-zoom-ratio-range #'- (current-buffer))
  (ffi-buffer-set-zoom-level buffer (current-zoom-ratio (current-buffer))))

(define-command reset-page-zoom (&key (buffer (current-buffer))
                                      (ratio (zoom-ratio-default (current-buffer))))
  "Reset the page zoom to the zoom-ratio-default."
  (ffi-buffer-set-zoom-level buffer (setf (current-zoom-ratio (current-buffer)) ratio)))

(define-internal-page-command-global summarize-buffer (&key (summary-length 5) (id (id (current-buffer))))
  (output (format nil "*Summary ~a*" (title (nyxt::buffers-get id))) 'base-mode)
  "Summarize the current buffer by creating a new summary buffer."
  (let ((buffer (nyxt::buffers-get id)))
    (let ((contents
            (serapeum:string-join
             (map 'list (lambda (e) (plump:text e))
                  (clss:select "p" (document-model buffer)))
             " ")))
      (spinneret:with-html-string
        (:style (style output))
        (:h1 "Summary for: " (title buffer))
        (:ul
         (loop for point in (analysis:summarize-text contents :summary-length summary-length)
               collect (:li point)))))))


(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs.")
   (keywords :documentation "Keywords associated with this heading.")
   (scroll-position :documentation "The scroll position of the heading."))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so that we
  can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defmethod prompter:object-attributes ((heading heading))
  `(("Title" ,(format nil "~a ~a"
                      (make-string (typecase (element heading)
                                     (nyxt/dom:h1-element 1)
                                     (nyxt/dom:h2-element 2)
                                     (nyxt/dom:h3-element 3)
                                     (nyxt/dom:h4-element 4)
                                     (nyxt/dom:h5-element 5)
                                     (nyxt/dom:h6-element 6)
                                     (t 0))
                                   :initial-element #\*)
                      (title heading)))
    ("Keywords" ,(format nil "~:{~a~^ ~}" (keywords heading)))))

(defun get-headings (&key (buffer (current-buffer)))
  (pflet ((heading-scroll-position
           (element)
           (ps:chain (nyxt/ps:qs-nyxt-id document (ps:lisp (get-nyxt-id element)))
                     (get-bounding-client-rect) y)))
    (with-current-buffer buffer
      (sort (map 'list
                 (lambda (e)
                   (make-instance 'heading :inner-text (plump:text e)
                                           :element e
                                           :buffer buffer
                                           :keywords (ignore-errors
                                                      (analysis:extract-keywords
                                                       (plump:text (plump:next-element e))))
                                           :scroll-position (heading-scroll-position e)))
                 (clss:select "h1, h2, h3, h4, h5, h6" (document-model buffer)))
            #'< :key (alex:compose #'parse-integer #'get-nyxt-id #'element)))))

(defun current-heading (&optional (buffer (current-buffer)))
  (alex:when-let* ((scroll-position (document-scroll-position buffer))
                   (vertical-scroll-position (second scroll-position))
                   (headings (get-headings :buffer buffer)))
    (first (sort headings
                 (lambda (h1 h2)
                   (< (abs (- (scroll-position h1) vertical-scroll-position))
                      (abs (- (scroll-position h2) vertical-scroll-position))))))))

(define-parenscript scroll-to-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier)))
            (scroll-into-view t)))

;; TODO: Make a method on plump:node? Extract to nyxt/dom?
(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (scroll-to-element :nyxt-identifier (get-nyxt-id (element heading))))

(define-command next-heading (&optional (buffer (current-buffer)))
  "Scroll to the next heading of the BUFFER."
  (sera:and-let* ((headings (get-headings :buffer buffer))
                  (current (current-heading buffer)))
    (scroll-page-to-heading (elt headings (1+ (position (element current) headings :key #'element))))))

(define-command previous-heading (&optional (buffer (current-buffer)))
  "Scroll to the previous heading of the BUFFER."
  (sera:and-let* ((headings (get-headings :buffer buffer))
                  (current (current-heading buffer)))
    (scroll-page-to-heading (elt headings (1- (position (element current) headings :key #'element))))))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:follow-p t)
   (prompter:follow-mode-functions #'scroll-page-to-heading)
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:actions (list (lambda-unmapped-command scroll-page-to-heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt
   :prompt "Jump to heading:"
   :sources (list (make-instance 'heading-source
                                 :buffer buffer))))

(define-command jump-to-heading-buffers ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6 across a set
of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers:"
                  :sources (make-instance 'buffer-source
                                          :multi-selection-p t
                                          :actions nil))))
    (prompt
     :prompt "Jump to heading:"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

(nyxt::define-panel-global headings ()
    (panel-buffer "*Headings panel*")
  "Display a list of heading for jumping."
  (labels ((get-level (heading)
             (ignore-errors (parse-integer (subseq (plump:tag-name (element heading)) 1))))
           (group-headings (headings)
             (loop with min-level = (apply #'min (mapcar #'get-level headings))
                   with current = (list)
                   for heading in headings
                   if (= (get-level heading) min-level)
                     collect (nreverse current) into total
                     and do (setf current (list heading))
                   else
                     do (push heading current)
                   finally (return (delete nil (append total (list (nreverse current)))))))
           (headings->html (groups)
             (spinneret:with-html-string
               (:ul
                (dolist (group groups)
                  (let ((heading (first group)))
                    (:li (:a :onclick
                             (ps:ps (nyxt/ps:lisp-eval
                                     `(progn
                                        (switch-buffer :id ,(id (buffer heading)))
                                        (scroll-to-element :nyxt-identifier
                                                           ,(get-nyxt-id (element heading))))))
                             (title heading)))
                    (when (rest group)
                      (:raw (sera:mapconcat #'headings->html (list (group-headings (rest group))) "")))))))))
    (ffi-window-set-panel-buffer-width (current-window) panel-buffer 400)
    (spinneret:with-html-string
      (:h1 "Headings")
      (:raw (headings->html (group-headings (get-headings)))))))

(pushnew 'web-mode nyxt::%default-modes)
