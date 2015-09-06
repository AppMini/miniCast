(ns minicast.utils
  (require [minicast.handler :refer [home-page]]))

; output the HTML as a string
(defn index-html []
  (print (apply str home-page)))
