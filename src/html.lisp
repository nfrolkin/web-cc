(in-package :web-cc)

(defmacro render-calculator (expression result)
  "Render calculator with EXPRESSION and RESULT."
  `(cl-markup:markup (:div (:form :method "post" :class "w3-row"
                                  (:div :class "w3-col l10 m10"
                                        (:input :class "w3-input"
                                                :type "text"
                                                :name "expression"
                                                :placeholder "Здесь должно быть выражение"
                                                :value ,expression))
                                  (:div :class "w3-col l2 m2 w3-center w3-hide-small"
                                        (:button :class "w3-btn w3-green" "Вычислить")))
                           (:div :class "w3-row-padding w3-large"
                                 (:div :class "w3-col l2 m2" (:p "Ответ:"))
                                 (:div :class "w3-col l2 m2" (:p ,result))))))

(defmacro render-page (styles scripts &rest body)
  "Render page with styling and scripting."
  `(let* ((cl-markup:*auto-escape* nil))
     (cl-markup:html (:head (:meta :charset "UTF-8")
                            (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                            ,@(loop for href in styles
                                 collect (list :link :rel "stylesheet" :href href))
                            ,@(loop for src in scripts
                                 collect (list :script :src src nil)))
                     (:body (:div :class "w3-content main" ,@body nil)))))
