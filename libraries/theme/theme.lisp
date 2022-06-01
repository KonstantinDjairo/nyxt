;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme)

(defvar theme nil
  "A dynamic variable to bind to current `theme' in `themed-css'.")

(defvar background nil
  "A dynamic variable to bind to `background-color' of the current `theme' in `themed-css'.")

(defvar on-background nil
  "A dynamic variable to bind to `text-color' of the current `theme' in `themed-css'.")

(defvar primary nil
  "A dynamic variable to bind to `primary-color' of the current `theme' in `themed-css'.")

(defvar on-primary nil
  "A dynamic variable to bind to `on-primary-color' of the current `theme' in `themed-css'.")

(defvar secondary nil
  "A dynamic variable to bind to `secondary-color' of the current `theme' in `themed-css'.")

(defvar on-secondary nil
  "A dynamic variable to bind to `secondary-color' of the current `theme' in `themed-css'.")

(defvar accent nil
  "A dynamic variable to bind to `accent-color' of the current `theme' in `themed-css'.")

(defvar on-accent nil
  "A dynamic variable to bind to `accent-color' of the current `theme' in `themed-css'.")

(defvar font-family nil
  "A dynamic variable to bind to `font-family' of the current `theme' in `themed-css'.")

(define-class theme ()
  ((dark-p
    nil
    :documentation "Whether the theme is dark.")
   (background-color
    "white"
    :type string
    :documentation "The background color of the theme.")
   (on-background-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of `background'.
Must contrast with `background'.")
   (primary-color
    "#555555"
    :type string
    :documentation "The main non-text/interface color.
Should preferably be neutral.")
   (on-primary-color
    "white"
    :type string
    :documentation "The color applied to elements appearing in front of `primary'.
Must contrast with `primary'.")
   (secondary-color
    "#A6A6A6"
    :type string
    :documentation "The secondary interface color.
Should contrast with `background-color'.")
   (on-secondary-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of `secondary'.
Must contrast with `secondary'.")
   (accent-color
    "#37a8e4"
    :type string
    :documentation "The color of highlighted elements that need attention.
Should stand out from all colors in the theme.")
   (on-accent-color
    "black"
    :type string
    :documentation "The color applied to elements appearing in front of `accent'.
Must contrast with `accent'.")
   (font-family
    "Helvetica Neue, Helvetica"
    :type string
    :documentation "The font family to use by default."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defvar +light-theme+
  (make-instance 'theme))

(defvar +dark-theme+
  (make-instance
           'theme
           :dark-p t
           :background-color "black"
           :on-background-color "white"
           :primary-color "#DB9665"
           :on-primary-color "black"
           ;; the relative contrast between this color and white is quite low.
           ;; I suggest #86451B.
           :secondary-color "#AD693E"
           :on-secondary-color "white"
           ;; for symmetric reasons, the accent should be chosen so that the
           ;; on-accent would be on-background (in this case, white).  I suggest
           ;; #735502.
           :accent-color "#FCBA04"
           :on-accent-color "black"))

(defun plist-p (object)
  "Return non-nil if OBJECT is a plist."
  (and (listp object)
       (alexandria:proper-list-p object)
       (evenp (length object))
       (loop :for x :in object :by #'cddr
             :always (keywordp x))))

(defun requote-rule (rule)
  (if (plist-p (rest rule))
      (cons 'list (mapcar (lambda (elem)
                            (typecase elem
                              (symbol (if (equalp "theme" (package-name (symbol-package elem)))
                                          elem
                                          `(quote ,elem)))
                              (atom (if (constantp elem)
                                        elem `(quote ,elem)))
                              (list elem)
                              (t elem)))
                          rule))
      (cons 'list (mapcar (lambda (x) `(quote ,x)) rule))))

(defmacro with-theme (theme &body body)
  "Evaluate body with the theme bindings available.
FIXME
The bindings are:
- `theme:theme' -- THEME itself.
- `theme:background' -- background color of the THEME.
- `theme:on-background' -- text color of the THEME.
- `theme:primary' -- primary color of the THEME.
- `theme:secondary' -- secondary color of the THEME.
- `theme:accent' -- accent color of the THEME.
- `theme:font' -- font family of the theme."
  `(let* ((theme:theme ,theme)
          (theme:background (background-color theme:theme))
          (theme:on-background (on-background-color theme:theme))
          (theme:primary (primary-color theme:theme))
          (theme:on-primary (on-primary-color theme:theme))
          (theme:secondary (secondary-color theme:theme))
          (theme:on-secondary (on-secondary-color theme:theme))
          (theme:accent (accent-color theme:theme))
          (theme:on-accent (on-accent-color theme:theme))
          (theme:font-family (font-family theme:theme)))
     ,@body))

(defmacro themed-css (theme &body rules)
  "Generate a CSS styled according to the THEME.

RULES is a list of CL-CSS rules (with one level of nesting removed). There are
special symbols that this macro will substitute for theme elements, if
encountered. See `with-theme' for a list of those.

Any non-atomic s-expression put as value of CSS property will be
evaluated too, so you can put arbitrarily complex expressions as
property values.

Example: color all the paragraph text in accent color if the theme is dark, and
in secondary color otherwise. Use the text color as background color. Make
headings have border of secondary color.

\(themed-css (make-instance 'theme
                            :dark-p t
                            :on-background-color \"red\"
                            :accent-color \"blue\")
           (|h1,h2,h3,h4,h5,h6|
            :border-style \"solid\"
            :border-width \"1px\"
            :border-color theme:secondary)
           (p
            :color (if (theme:dark-p theme:theme) theme:accent theme:secondary)
            :background-color theme:background))"
  `(with-theme ,theme
    (cl-css:css
     (list ,@(mapcar #'requote-rule rules)))))
