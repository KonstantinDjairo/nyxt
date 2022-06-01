;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :theme/tests)

(prove:plan nil)

(defvar *theme* (make-instance
                 'theme:theme
                 :dark-p t
                 :background-color "black"
                 :on-background-color "white"
                 :accent-color "magenta"
                 :primary-color "yellow"
                 :secondary-color "blue")
  "An absolutely crazy theme to test things on.")

;; FIXME change the colors based on background-color and rewrite the output
(prove:subtest "Basic CSS substitution"
  (prove:is (theme:themed-css *theme*
              (a
               :background-color theme:background
               :color theme:primary))
            "a { color: yellow; background-color: black; }
"))

(prove:subtest "Multi-rule/multi-color substitution"
  (prove:is (theme:themed-css *theme*
              (a
               :background-color theme:background
               :color theme:primary)
              (body
               :background-color theme:primary
               :color theme:on-background)
              (h1 :color theme:accent))
            "a { color: yellow; background-color: black; }
body { color: white; background-color: yellow; }
h1 { color: magenta; }
"))

(prove:subtest "Inline function execution"
  (prove:is  (theme:themed-css *theme*
               (body
                :color (concatenate 'string theme:accent " !important")
                :background-color theme:primary))
             "body { color: magenta !important; background-color: yellow; }
"))

(prove:subtest "Inline macro/special form invokation"
  (prove:is (theme:themed-css *theme*
              (body
               :color (if (theme:dark-p theme:theme) theme:background theme:on-background)
               :background-color theme:primary))
            "body { color: black; background-color: yellow; }
"))

(prove:finalize)
