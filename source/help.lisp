;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun configure-slot (slot class &key
                                    (type (getf (mopu:slot-properties (find-class class) slot)
                                                :type)))
  "Set the value of a slot in `*auto-config-file*'.
CLASS is a class symbol."
  (sera:nlet lp ()
    (let ((input (read-from-string
                  (prompt1
                    :prompt (format nil "Configure slot value ~a" slot)
                    :sources (make-instance 'prompter:raw-source)))))
      (cond
        ((and type (not (typep input type)))
         (echo-warning "Type mismatch for ~a: got ~a, expected ~a."
                       slot (type-of input) type)
         (lp))
        (t
         (auto-configure :class-name class :slot slot :slot-value input)
         (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input))))))

(define-internal-page-command-global common-settings ()
    (buffer "*Settings*" 'nyxt/help-mode:help-mode)
  "Configure a set of frequently used settings."
  (spinneret:with-html-string
    (:h1 "Common Settings")
    (:p "Set the values for frequently configured settings. "
        "Changes only apply to newly created buffers.")
    (:h2 "Keybinding style")
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/emacs-mode:emacs-mode :activate nil :buffer input-buffer))
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/vi-mode:vi-normal-mode :activate nil :buffer input-buffer)))))
                 "Use default (CUA)"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/vi-mode:vi-normal-mode :activate nil :buffer input-buffer))
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/emacs-mode:emacs-mode :activate t :buffer input-buffer)))))
                 "Use Emacs"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/emacs-mode:emacs-mode :activate nil :buffer input-buffer))
                                     (nyxt::auto-configure
                                      :class-name 'input-buffer
                                      :form '(nyxt/vi-mode:vi-normal-mode :activate t :buffer input-buffer)))))
                 "Use vi"))
    (flet ((generate-colors (theme-symbol text)
             (spinneret:with-html-string
               (:p (:button :class "button"
                            :style (format nil "background-color: ~a; color: ~a"
                                           (theme:primary-color (symbol-value theme-symbol))
                                           (theme:background-color (symbol-value theme-symbol)))
                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                             `(nyxt::auto-configure
                                               :class-name 'browser
                                               :slot 'theme
                                               :slot-value ',theme-symbol)))
                            text))
               (:p "Colors:")
               (:dl
                (loop for (name color text-color) in '(("Background" theme:background-color theme:on-background-color)
                                                       ("On Background" theme:on-background-color theme:background-color)
                                                       ("Accent" theme:accent-color theme:on-accent-color)
                                                       ("On Accent" theme:on-accent-color theme:accent-color)
                                                       ("Primary" theme:primary-color theme:on-primary-color)
                                                       ("On Primary" theme:on-primary-color theme:primary-color)
                                                       ("Secondary" theme:secondary-color theme:on-secondary-color)
                                                       ("On Secondary" theme:on-secondary-color theme:secondary-color))
                      collect (:dt name ": ")
                      collect (:dd (:span :style (format nil "background-color: ~a; color: ~a; border-radius: 0.2em"
                                                         (slot-value (symbol-value theme-symbol) color)
                                                         (slot-value (symbol-value theme-symbol) text-color))
                                          (slot-value (symbol-value theme-symbol) color))))))))
      (:h2 "Theme style")
      (:ul
       (:li (:raw (generate-colors 'theme::+light-theme+ "Use default (Light theme)")))
       (:li (:raw (generate-colors 'theme::+dark-theme+ "Use Dark theme")))))
    (:h2 "Miscellaneous")
    (:ul
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(nyxt::configure-slot 'default-new-buffer-url 'browser :type 'string)))
                   "Set default new buffer URL"))
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(nyxt::configure-slot 'current-zoom-ratio 'document-buffer)))
                   "Set default zoom ratio"))
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(nyxt::auto-configure
                                      :form '(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1"))))
                   "Disable compositing")
          (:p "On some systems, compositing can cause issues with rendering. If
you are experiencing blank web-views, you can try to disable compositing. After
disabling compositing, you will need to restart Nyxt."))

     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    '(nyxt::edit-user-file-with-external-editor)))
                   "Edit user files")
          (:p "Edit user configuration and other files in external text editor.")))))

(define-command print-bindings-cheatsheet ()
  "Print the buffer with the list of all known bindings for the current buffer
optimizing the use of space."
  (nyxt::html-set-style
   (theme:themed-css (theme *browser*)
     (h3 :font-size "10px"
         :font-family theme:font-family
         :font-weight 500)
     (tr :font-size "7px")
     (div :display inline-block))
   (describe-bindings))
  (nyxt/document-mode:print-buffer))

(defun tls-help (buffer url)
  "This function is invoked upon TLS certificate errors to give users
help on how to proceed."
  (setf (slot-value buffer 'status) :failed)
  (html-set
   (spinneret:with-html-string
     (:h1 (format nil "TLS Certificate Error: ~a" (render-url url)))
     (:p "The address you are trying to visit has an invalid
certificate. By default Nyxt refuses to establish a secure connection
to a host with an erroneous certificate (e.g. self-signed ones). This
could mean that the address you are attempting the access is
compromised.")
     (:p "If you trust the address nonetheless, you can add an exception
for the current hostname with the "
         (:code "add-domain-to-certificate-exceptions")
         " command.  The "
         (:code "certificate-exception-mode")
         " must be active for the current buffer (which is the
default).")
     (:p "To persist hostname exceptions in your initialization
file, see the "
         (:code "add-domain-to-certificate-exceptions")
         " documentation."))
   buffer))

(define-command nyxt-version ()
  "Version number of this version of Nyxt.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(define-internal-page-command-global new ()
    (buffer "*New buffer*")
  "Open up a buffer with useful links suitable for a `default-new-buffer-url'."
  (spinneret:with-html-string
    (:style (:raw (theme:themed-css (theme *browser*)
                    (body
                     :min-height "100vh")
                    (nav
                     :text-align "center"
                     :top 0)
                    (details
                     :display "inline"
                     :margin "1em")
                    (h1
                     :font-size "5em"
                     :margin "0.1em")
                    (main
                     :padding "10%"
                     :text-align "center"
                     :display "flex"
                     :flex-direction "column"
                     :justify-content "center")
                    (.centered
                     :text-align "center")
                    (.button
                     :min-width "100px")
                    (.container
                     :min-height "100%")
                    (.copyright
                     :position "absolute"
                     :bottom "1em"
                     :right "1em"))))
    (:div
     :class "container"
     (:nav
      :class "centered"
      (:a :class "button" :href (nyxt-url 'tutorial)
          :title "An introduction to Nyxt core concepts."
          "Tutorial")
      (:a :class "button" :href (nyxt-url 'manual)
          :title "Full documentation about Nyxt, how it works and how to configure it."
          "Manual")
      (:a :class "button" :href (nyxt-url 'changelog)
          :title "Information about changes between Nyxt versions."
          "Change Log")
      (:a :class "button" :href (nyxt-url 'describe-bindings)
          :title "List all bindings for the current buffer."
          "List bindings")
      (:a :class "button" :href (nyxt-url 'common-settings)
          :title "Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level."
          "⚙ Settings")
      (:details
       (:summary :class "button" "Other useful links")
       (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/"
           :title "Your contribution will be much appreciated :)"
           "Source Code")
       (:a :class "button" :href "https://www.youtube.com/channel/UC11mZYESUvaKFTaa3zZWHMQ"
           :title "A channel with tips and tricks of Nyxt by one of the developers."
           "Nyxt Academy")
       (:a :class "button" :href "https://nyxt.atlas.engineer/articles"
           :title "Learn more about why's and how's behind Nyxt features."
           "Articles")
       (:a :class "button" :href "https://nyxt.atlas.engineer/applications"
           :title "Check out the applications built on top of Nyxt!"
           "Applications")
       (:a :class "button" :href "https://store.nyxt.atlas.engineer/"
           :title "Buy Nyxt merchandise and support the development!"
           "Store")
       (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/blob/master/documents/README.org"
           :title "Helpful tips for Nyxt hacking and contributing."
           "Developer Manual")
       (:a :class "button" :href "https://discourse.atlas.engineer/"
           :title "A forum for questions and ideas on Nyxt."
           "Forum")
       (:a :class "button" :href "https://kiwiirc.com/nextclient/irc.libera.chat/nyxt"
           :title "Chat with developers and other Nyxt users."
           "Chat")))
     (:main
      (:h1 "Nyxt")
      (:i "The Internet on your terms.")
      (:p (:button :class "button accent"
                   :type "submit"
                   :onclick (ps:ps (nyxt/ps:lisp-eval '(set-url :prefill-current-url-p nil)))
                   "Start searching!")))
     (:p :class "copyright"
         (format nil "Nyxt/~a ~a" +renderer+ +version+)
         (:br)
         (format nil "Atlas Engineer LLC, 2018-~a" (local-time:timestamp-year (local-time:now)))))))

(sera:eval-always ; To satisfy `fboundp' of `manual' at compile-time (e.g. CCL).
  (define-internal-page-command-global manual ()
      (buffer "*Manual*" 'nyxt/help-mode:help-mode)
    "Show the manual."
    (spinneret:with-html-string (:style (style buffer))
      (:style (cl-css:css '(("body"
                             :max-width "80ch"))))
      (:raw (manual-content)))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/help-mode:help-mode)
  "Show the tutorial."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (cl-css:css '(("body"
                           :max-width "80ch"))))
    (:h1 "Nyxt tutorial")
    (:p "The following tutorial introduces core concepts and
basic usage.  For more details, especially regarding configuration, see
the "
        (:code (command-markup 'manual)) ".")
    (:raw (tutorial-content))))

(define-internal-page-command-global show-system-information ()
    (buffer "*System information*")
  "Show buffer with Lisp version, Lisp features, OS kernel, etc.
System information is also saved into the clipboard."
  (let* ((*print-length* nil)
         (nyxt-information (system-information)))
    (prog1
        (spinneret:with-html-string
          (:h1 "System information")
          (:pre nyxt-information))
      (copy-to-clipboard nyxt-information)
      (log:info nyxt-information)
      (echo "System information copied to clipboard."))))

(define-internal-page-command-global dashboard ()
    (buffer "*Dashboard*")
  "Print a dashboard."
  (flet ((list-bookmarks (&key (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/bookmark-mode:bookmark-mode)))
               (or (let ((bookmarks (files:content (nyxt/bookmark-mode:bookmarks-file mode))))
                     (loop for bookmark in bookmarks
                           collect (:li (title bookmark) separator
                                        (:a :href (render-url (url bookmark))
                                            (render-url (url bookmark))))))
                   (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/bookmark-mode:bookmarks-file mode)))))))))
    (let ((dashboard-style (theme:themed-css (theme *browser*)
                             (body
                              :background-color theme:background
                              :color theme:on-background
                              :margin-top 0
                              :margin-bottom 0)
                             ("#title"
                              :font-size "400%")
                             ("#subtitle"
                              :color theme:secondary)
                             (.section
                              :border-style "solid none none none"
                              :border-color theme:secondary
                              :margin-top "10px"
                              :overflow "scroll"
                              :min-height "150px")
                             ("h3"
                              :color theme:secondary)
                             ("ul"
                              :list-style-type "circle"))))
      (spinneret:with-html-string
        (:style dashboard-style)
        (:div
         (:h1 :id "title" "Nyxt " (:span :id "subtitle" "browser ☺"))
         (:h3 (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
         (:button :class "button" :onclick (ps:ps (nyxt/ps:lisp-eval
                                                   `(nyxt::restore-history-by-name)))
                  "🗁 Restore Session")
         (:a :class "button" :href (nyxt-url 'manual) "🕮 Manual")
         (:button :class "button"
                  :onclick (ps:ps (nyxt/ps:lisp-eval `(nyxt::execute-command)))
                  "≡ Execute Command")
         (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
        (:h3 (:b "Bookmarks"))
        (:ul (:raw (list-bookmarks)))
        (:h3 (:b "Recent URLs"))
        (:ul (:raw (history-html-list)))))))
