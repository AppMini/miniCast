(ns minicast.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<!]])
    (:import goog.History))

(defonce app-state (atom nil))
(defonce errors (atom []))
(defonce auth-state (atom nil))

(def server-url "http://localhost:8000/server/")

;; -------------------------
;; Components

(defn component-errors []
  (if (not (empty? @errors))
    [:div {:class "errors" :on-click (fn [ev] (reset! errors nil))}
      (map-indexed (fn [i e] [:div {:class "error" :key (str "error-" i)} [:i {:class "fa fa-warning"}] e]) @errors)]))

(defn component-loader []
  [:div {:class "loader-inner line-scale-pulse-out-rapid"}
    [:div] [:div] [:div] [:div] [:div]])

(defn component-logo []
  [:div
    [:div {:class "fog"}]
    [:div {:class "logo"} "mini" [:b "Cast"]]])

;; -------------------------
;; Helper functions

(defn ajax-error-handler [{:keys [status status-text]}]
  (swap! errors conj (str "Oops: " status " " status-text)))

;; -------------------------
;; Views

(defn home-page []
  (fn []
    [:div
     (component-errors)
     (if (nil? @app-state)
        [:div
          (component-logo)
          (component-loader)]
        [:div "Have state."])]))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; History
;; must be called after routes have been defined

(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))

; make the request to get our state
(go (let [response (<! (http/get (str server-url "state.php") {:with-credentials? false}))]
  (reset! auth-state (js/JSON.parse (:body response)))))

