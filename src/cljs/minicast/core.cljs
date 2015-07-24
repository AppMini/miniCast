(ns minicast.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [minicast.store :refer [remember! forget! recall]]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [ajax.core :refer [GET POST ajax-request json-response-format raw-response-format]]
              [cljs.core.async :refer [<!]])
    (:import goog.History))

; set the app state from last-saved localstorage
(defonce app-state (atom (recall "app-state")))
; collect errors to show to the user
(defonce errors (atom []))
; is the user logged in?
(defonce auth-state (atom "AUTH_UNKNOWN"))
; count of urls currently in the syncing state
(defonce urls-syncing (atom 0))

(enable-console-print!)

(def server-url "http://localhost:8000/server/index.php")

(print "app-state @ launch:" @app-state)

;; -------------------------
;; Helper functions

; log a single error message
(defn log-error [e]
  (swap! errors conj e))

; if we hit an error loading an ajax endpoint
(defn ajax-error-handler [{:keys [status status-text]}]
  (log-error (str "Oops: " status " " status-text)))

; get the body of the returned request regardless of whether it was an error or not
(defn get-body [ok result] (if ok result (:response result)))

; update the authentication state token after a request
(defn updated-server-state-handler [params]
  (fn [[ok result]]
    (do
      (print "auth state ok:" ok)
      (print "auth state result:" result)
      (print "auth state body:" (get-body ok result))
      ; if we get a 404 from the server without the server sending back the api-error key it isn't installed
      (cond
        (or (and
              ; if we didn't receive an api error
              (not (contains? (get-body ok result) "api-error"))
              ; but we did receive a 404
              (= (:status result) 404))
            ; or some other non-server error happened
            (= (:status result) 0))
          (reset! auth-state "AUTH_SERVER_NOT_FOUND")
      
      ; if we got a special error response from the API
      (contains? (get-body ok result) "api-error")
        (let [s (get (get-body ok result) "api-error")]
          (if (= (.indexOf s "AUTH") 0)
            (reset! auth-state s)
            (log-error (str "Error talking to the server: " s " see console for more details."))))
      
      ; if we got a special success response from the API
      (contains? (get-body ok result) "api")
        (let [s (get (get-body ok result) "api")]
          (if (= (.indexOf s "AUTH") 0)
            (reset! auth-state s)
            (print "API success:" s)))
      
      ; if we got a legitimate state
      true
        (let [new-app-state (get-body ok result)]
          (if (and (not (nil? new-app-state)) (contains? new-app-state :app-state))
            (do
              ; TODO: merge the existing state (from localstorage) with server state
              ; https://clojuredocs.org/clojure.core/assoc-in#example-548a3809e4b04e93c519ffa4
              (print "Updating state to" new-app-state)
              (reset! app-state (:app-state new-app-state)))))))))

; unified interface for access to our api
(defn api-request [params & callback]
  (ajax-request (merge {:uri server-url :method :get :with-credentials true :response-format (json-response-format) :handler (updated-server-state-handler params)} {:params params})))

; special call to the proxy request endpoint
(defn proxy-request [url callback]
  (ajax-request {:uri server-url :params {:proxy url} :method get :with-credentials true :response-format (raw-response-format) :handler callback}))

; initiate the request for user's current state
(defn request-app-state []
  (api-request {:state ""}))

; watch the auth state and if we get AUTHENTICATED then request app-state
(add-watch auth-state :auth-state-watcher
  (fn [key atom old-state new-state]
      (if (and (not (= old-state new-state)) (= new-state "AUTHENTICATED")) (request-app-state))))

; initiate the request for the current authentication state
(defn request-authentication-state []
  (api-request {:auth ""}))

; submit the request to log out
(defn submit-logout-request []
    (api-request {:logout true}))

; submit the request to log in
(defn submit-user-pass-form [un pw]
  ; tell the server the username and password to create pass/log in
  (api-request {:auth true :username @un :password @pw}))

; redirect to the longin page
(defn redirect [url]
  (set! (-> js/document .-location .-href) url))

;; -------------------------
;; data sync

; watch the app-state atom for changes and write-back to the server on change
(add-watch app-state :app-state-watcher
  (fn [key atom old-state new-state]
    (if (not (= old-state new-state))
      (print "Writing new app-state to localstorage and the server:")
      (print new-state)
      (remember! "app-state" new-state)
      (api-request {:state {:app-state new-state}}))))

; make calls to the podcast endpoints one by one
(defn sync-urls [syncing]
  (go
    (if (= @syncing 0)
      (reset! syncing 3))
    (print @syncing)
    (swap! syncing dec)
    (print @syncing)))

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
       [:div {:class "buttonbar"}
         [:button {:on-click #(submit-user-pass-form un pw)} [:i {:class "fa fa-check"}]]]]]))

(defn component-auth-configured []
  [:div
    [:div {:class "buttonbar"}
      [:button {:title "logout" :on-click submit-logout-request} [:i {:class "fa fa-sign-out"}]]
      [:button {:title "home" :on-click #(redirect "#/")} [:i {:class "fa fa-home"}]]]
    [:p [:i {:class "fa fa-check tick"}] "Successfully connected to the sync backend."]])

(defn component-setup-server-info []
  [:div [:p {:class "error"} "Could not contact the server. Please install the " [:a {:href "https://github.com/chr15m/pellet"} "pellet"] " server into a folder called 'server'." [:p "If you're using git then you should be able to run:"] [:pre [:code "git submodule init\n"  "git submodule update"] ] [:p "Or clone the repository again using:"] [:code "git clone --recursive https://github.com/chr15m/miniCast"]]])

(let [show-add-url-box (atom false) url-to-add (atom "")]
  (defn component-urls-config []
    [:div {:class "buttonbar"}
      [:button {:title "add podcast" :on-click #(swap! show-add-url-box not)} [:i {:class (str "fa " (if @show-add-url-box "fa-close" "fa-plus"))}]]
      (if @show-add-url-box
        [:input {:placeholder "https://www.astronomycast.com/feed/" :class "url" :type "uri" :value @url-to-add :on-change #(reset! url-to-add (-> % .-target .-value))}])]))

;; -------------------------
;; Views

(defn home-page []
  (if (case @auth-state "AUTHENTICATED" true nil true false)
    (fn []
      [:div {:class "buttonbar"}
        [:button {:title "refresh" :on-click #(if (= @urls-syncing 0) (sync-urls urls-syncing))} [:i {:class (str "fa fa-refresh" (if (> @urls-syncing 0) " fa-spin spin-2x" ""))}]]
        [:button {:title "settings" :on-click #(redirect "#/sync-config")} [:i {:class "fa fa-cog"}]]])
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
          "AUTH_SERVER_NOT_FOUND" (component-setup-server-info)
          "AUTH_NO_FILE" (component-user-pass "firstrun" "No authentication has been set up yet. Create a new username and password:")
          "AUTH_FAILED" [:div [:p "Incorrect username/password."] (component-user-pass "login" "")]
          "AUTH_FILE_CREATED" [:div [:p {:class "info"} "Authentication file created successfully."] (component-user-pass "login" "")]
          "AUTH_NO_CREDENTIALS" (component-user-pass "login" "")
          "AUTH_LOGGED_OUT" [:div [:p "You have been logged out."] (component-user-pass "login" "")]
          "AUTHENTICATED" (component-auth-configured)
          nil (component-auth-configured)
          nil)
        [:div {:class "debug"} "Debug: " a]
        ; if logged in show url configuration
        (case a
          "AUTHENTICATED" (component-urls-config)
          nil (component-urls-config)
          nil)]))

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
(request-authentication-state)

