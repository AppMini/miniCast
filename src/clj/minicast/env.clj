(ns minicast.env
  (:require [environ.core :refer [env]]))

(defmacro get-env [kw]
  (env kw))

