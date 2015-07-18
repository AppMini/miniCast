(ns minicast.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [minicast.store :refer [remember! forget! recall]]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [ajax.core :refer [GET POST ajax-request json-response-format]]
              [cljs.core.async :refer [<!]])
    (:import goog.History))

; set the app state from last-saved localstorage
(defonce app-state (atom (recall "app-state")))
; collect errors to show to the user
(defonce errors (atom []))
; is the user logged in?
(defonce auth-state (atom "AUTH_UNKNOWN"))

(enable-console-print!)

(def server-url "http://localhost:8000/server/")

;; -------------------------
;; Helper functions

; log a single error message
(defn log-error [e]
  (swap! errors conj e))

; if we hit an error loading an ajax endpoint
(defn ajax-error-handler [{:keys [status status-text]}]
  (log-error (str "Oops: " status " " status-text)))

; unified interface for access to our api
(defn api-request [endpoint params]
  (ajax-request (merge {:uri (str server-url endpoint ".php") :method :get :with-credentials true :response-format (json-response-format)} params)))

; get the body of the returned request regardless of whether it was an error or not
(defn get-body [ok result] (if ok result (:response result)))

; update the authentication state token after a request
(defn update-auth-state-handler [[ok result]] (do (print "auth state:" ok result) (if (= (:status result) 404) (reset! auth-state "AUTH_NOT_FOUND") (reset! auth-state (get-body ok result)))))

; initiate the request for user's current state
(defn request-state []
  (api-request "state" {:handler update-auth-state-handler}))

; submit the request to log out
(defn submit-logout-request []
    (api-request "auth" {:params {:logout true} :handler update-auth-state-handler}))

; submit the request to log in
(defn submit-user-pass-form [un pw]
  ; tell the server the username and password to create pass/log in
  (api-request "auth" {:params {:username @un :password @pw} :handler update-auth-state-handler}))

; redirect to the longin page
(defn redirect [url]
  (set! (-> js/document .-location .-href) url))

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

(let [un (atom "") pw (atom "")]
  (defn component-user-pass [formclass message]
    [:div {:class formclass}
     [:div
       [:p message]
       [:input {:placeholder "username" :type "text" :value @un :on-change #(reset! un (-> % .-target .-value))}]
       [:input {:type "password" :value @pw :placeholder "password" :on-change #(reset! pw (-> % .-target .-value))}]
       [:button {:on-click #(submit-user-pass-form un pw)} "Go"]]]))

(defn component-auth-configured []
  [:div [:p [:i {:class "fa fa-check tick"}] "Successfully connected to the sync backend."]
    [:button {:on-click submit-logout-request} "Logout"]
    [:button {:on-click (fn [] (redirect "#/"))} "Ok"]])

(defn component-setup-server-info []
  [:div [:p {:class "error"} "Could not contact the server. Please install the " [:a {:href "https://github.com/chr15m/pellet"} "pellet"] " server into a folder called 'server'." [:p "If you're using git then you should be able to run:"] [:pre [:code "git submodule init\n"  "git submodule update"] ] [:p "Or clone the repository again using:"] [:code "git clone --recursive https://github.com/chr15m/miniCast"]]])

;; -------------------------
;; Views

(defn home-page []
  (if (case @auth-state "AUTHENTICATED" true nil true false)
    (fn []
      [:div "Home page"])
    ; the user isn't logged in or hasn't set up sync - redirect to sync setup page.
    (do
      (redirect "#/sync-config")
      [:div "Redirecting to sync config."])))

(defn sync-config-page []
  (let [a @auth-state]
      [:div
        ; display any errors received to the user
        (component-errors)
        ; display the logo on this configuration page
        (component-logo)
        ; show a loader if we're still loading the auth state
        (if (= a "AUTH_UNKNOWN") (component-loader))
        ; the actual state flow
        (case a
          "AUTH_NOT_FOUND" (component-setup-server-info)
          "AUTH_NO_FILE" (component-user-pass "firstrun" "No authentication has been set up yet. Create a new username and password:")
          "AUTH_FAILED" [:div [:p "Incorrect username/password."] (component-user-pass "login" "Login:")]
          "AUTH_FILE_CREATED" [:div [:p {:class "info"} "Authentication file created successfully."] (component-user-pass "login" "Login:")]
          "AUTH_NO_CREDENTIALS" (component-user-pass "login" "Login:")
          "AUTH_LOGGED_OUT" [:div [:p "You have been logged out."] (component-user-pass "login" "Login:")]
          "AUTHENTICATED" (component-auth-configured)
          nil (component-auth-configured)
          nil)
        [:div {:class "debug"} "Debug: " a]]))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/sync-config" []
  (session/put! :current-page #'sync-config-page))

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

; make the request to get our state from the server
(request-state)

