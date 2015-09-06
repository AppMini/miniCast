(ns minicast.core
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [minicast.store :refer [remember! forget! recall]]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [ajax.core :refer [GET POST ajax-request json-response-format raw-response-format url-request-format]]
              [tubax.core :refer [xml->clj]]
              [cljs.pprint :refer [pprint]]
              [cljs.core.async :refer [<! chan close! put!]])
    (:import goog.History))

; set the app state from last-saved localstorage
(defonce app-state (atom (or (recall "app-state") {})))
; remember where different podcasts are up to (debounce - this gets written to state)
(defonce play-position (atom {}))
; collect errors to show to the user
(defonce errors (atom []))
; count of urls currently in the syncing state
(def urls-syncing (atom nil))
; is the user logged in?
(def auth-state (atom "AUTH_UNKNOWN"))

(enable-console-print!)

(def server-url "server/index.php")

(print "app-state @ launch:" @app-state)

;; -------------------------
;; Helper functions

; log a single error message
(defn log-error [e]
  (swap! errors conj e))

; current timestamp
(defn get-now []
  (.getTime (js/Date.)))

; if we hit an error loading an ajax endpoint
(defn ajax-error-handler [{:keys [status status-text]}]
  (log-error (str "Oops: " status " " status-text)))

; get the body of the returned request regardless of whether it was an error or not
(defn get-body [ok result] (if ok result (:response result)))

; swap! to remove a uri from app-state
(defn remove-uri [old-app-state uri]
  (update-in old-app-state ["uris"]
             (fn [old-uris] (vec (remove (fn [i] (= (i "uri") uri)) old-uris)))))

; get index of the uri in the list of all of them
(defn get-uri-index [uri]
  (last (find (apply merge (map-indexed (fn [i e] {(e "uri") i}) (get-in @app-state ["uris"]))) uri)))

; get the uri structure indexed on the uri
(defn get-uri-map [uri]
  (first (filter (fn [e] (= (e "uri") uri)) (get-in @app-state ["uris"]))))

; updates some values on the map that contains the uri
(defn update-uri [old-app-state uri & args]
  (let [updated (merge (get-uri-map uri) (apply hash-map args))]
    (assoc-in old-app-state ["uris" (get-uri-index uri)] updated)))

; swap! to add a uri to app-state
(defn add-uri [old-app-state uri]
  (let [uri-struct {"timestamp" (get-now) "uri" uri}]
    (if (nil? (old-app-state "uris"))
      ; just jam a completely new one in there
      (assoc-in old-app-state ["uris"] [uri-struct])
      ; if we already have this one, first remove it
      (let [old-app-state-no-dups (remove-uri old-app-state uri)]
        ; then add the new one to replace it
        (assoc-in old-app-state-no-dups ["uris" (count (old-app-state-no-dups "uris"))] uri-struct)))))

; merge incoming app-state urls from the server with the ones we already have
(defn merge-new-state [old-app-state new-app-state]
  ;(update-in (merge old-app-state new-app-state) ["uris"] (old-app-state "uris"))
  new-app-state)

; test for valid json
(defn is-valid-json [j]
  (try (js/JSON.parse j)
       true
       (catch :default e false)))

; check and update the app state to reflect auth state coming from the server
(defn check-auth-state [[ok result]]
  (do
    ;(print "auth state ok:" ok)
    ;(print "auth state result:" result)
    ;(print "auth state body:" (get-body ok result))
    (cond
      ; basic check - 403 means not authenticated
      (and (not (is-valid-json (:response result))) (= (:status result) 403))
        (reset! auth-state "AUTH_NO_CREDENTIALS")

      ; if we get a 404 from the server without the server sending back the api-error key it isn't installed
      (and (is-valid-json (:response result)) (or (and
            ; if we didn't receive an api error
            (not (contains? (get-body ok result) "api-error"))
            ; but we did receive a 404
            (= (:status result) 404))
          ; or some other non-server error happened
          (= (:status result) 0)))
        (reset! auth-state "AUTH_SERVER_NOT_FOUND")
    
      ; if we got a special error response from the API
      (and (is-valid-json (:response result)) (contains? (get-body ok result) "api-error"))
        (let [s (get (get-body ok result) "api-error")]
          (if (= (.indexOf s "AUTH") 0)
            (reset! auth-state s)
            (log-error (str "Error talking to the server: " s " see console for more details."))))
    
      ; if we got a special success response from the API
      (and (is-valid-json (:response result)) (contains? (get-body ok result) "api"))
        (let [s (get (get-body ok result) "api")]
          (if (= (.indexOf s "AUTH") 0)
            (reset! auth-state s)
            (print "API success:" s)))
    
    ; if we got a legitimate state return true
    true true)))

; update the authentication state token after a request
(defn updated-server-state-handler [params]
  (fn [[ok result]]
    (if (check-auth-state [ok result])
      (let [new-app-state (get-body ok result)]
          (if (and (not (nil? new-app-state)) (contains? new-app-state "app-state"))
            (do
              (print "new-app-state" new-app-state)
              ; merge the existing state (from localstorage) with server state
              (swap! app-state merge-new-state (new-app-state "app-state"))))))))

; unified interface for access to our api
(defn api-request [params & config]
  (let [request (merge {:uri server-url
                        :method :get
                        :with-credentials true
                        :response-format (json-response-format)
                        :handler (updated-server-state-handler params)} {:params params} (if (count config) (first config) {}))]
    (ajax-request request)))

; special call to the proxy request endpoint
(defn proxy-request [url callback]
  (ajax-request {:uri server-url
                 :params {:proxy url}
                 :method :get
                 :with-credentials true
                 :response-format (json-response-format)
                 :handler (fn [[ok result]]
                            (print "proxy-request" ok url)
                            ; check the auth status in case they have been logged out
                            (check-auth-state [ok result])
                            ; always run the callback so the go blocks will finish
                            (callback [ok result]))}))

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

; toggle the color scheme
(defn toggle-scheme [old-app-state]
  (assoc-in old-app-state ["scheme"]
    (let [scheme (old-app-state "scheme")]
      (if (nil? scheme)
        ; default first value is 'night'
        "night"
        ; otherwise toggle
        (if (= scheme "day") "night" "day")))))

; redirect to the login page
(defn redirect [url]
  (set! (-> js/document .-location .-href) url))

; duh - tubax library does all this already better

; find certain tags within an xml structures
(defn find-tag [xml tag]
  (filter #(= (get % :tag) tag) xml))

; get a particular item's tag value
(defn get-item-tag [item tag]
  (-> item :content (find-tag tag) first :content first))

; find the image tag in a podcast rss
(defn podcast-find-image [contents]
  (or
    ; look for image tag
    (-> contents (find-tag :image) first :content (find-tag :url) first :content first)
    ; look for itunes image tag
    (-> contents (find-tag :itunes:image) first :attributes :href)))

; find the title in a podcast rss
(defn podcast-find-title [contents]
  (or
    ; look for image -> title tag
    (-> contents (find-tag :image) first :content (find-tag :title) first :content first)
    ; look for the actual title tag
    (-> contents (find-tag :title) first :content first)))

; make a hash-map "json friendly" by turning all of the keywords into names
(defn json-friendly [m]
  (into {}
    (for [[k v] m]
      [(keyword k) v])))

;; -------------------------
;; data sync

; wrap callbacks in a channel
; http://www.lispcast.com/core-async-code-style
(defn <<< [f & args]
  (let [c (chan)]
    (apply f (concat args [(fn [x]
                             (if (or (nil? x)
                                     (undefined? x))
                                   (close! c)
                                   (put! c x)))]))
    c))

; watch the app-state atom for changes and write-back to the server on change
(add-watch app-state :app-state-watcher
  (fn [key atom old-state new-state]
    (if (not (= old-state new-state))
      (do
        (print "Writing new app-state to localstorage and the server:")
        (print new-state)
        (remember! "app-state" new-state)
        (api-request {:state (js/JSON.stringify (clj->js {:app-state new-state}))} {:method :post :format (url-request-format)})))))

; make calls to the podcast endpoints one by one
(defn sync-urls [syncing]
  ; set up our array of syncing things
  (doseq [u (@app-state "uris")] (swap! syncing conj (u "uri")))
  ; go nuts with the async ajax requests
  (let [requests (map (fn [u] [u (<<< proxy-request u)]) @syncing)]
    (go
      (doseq [[uri chan] requests]
        (let [[ok response] (<! chan)]
          ; check the ajax result code and sanity check for xml
          (if (and ok (string? response))
            (let [rss (xml->clj response {:strict false})
                  contents (get-in rss [:content 0 :content])
                  items (-> contents (find-tag :item))
                  image (podcast-find-image contents)
                  title (podcast-find-title contents)]
              (if (= (count items) 0) (log-error (str uri " contains no podcasts.")))
              ; update the latest version of the podcast's reference image
              (swap! app-state update-uri uri "image-uri" image)
              (swap! app-state update-uri uri "title" title)
              ; ensure we have a list of podcasts in our app state
              (if (nil? (@app-state "podcasts"))
                ; just jam a completely new one in there
                (swap! app-state assoc-in ["podcasts"] []))
              ; add any new podcasts we find in the loop of items to our master list
              (swap! app-state update-in ["podcasts"] (fn [old-podcasts new-podcasts] (take 100 (reverse (sort-by #(js/Date. (% "timestamp")) (concat old-podcasts (vec new-podcasts))))))
                ; filter out the nil values
                (remove nil?
                  ; loop through all of the items we received
                  (for [i items]
                    (let [guid (get-item-tag i :guid)]
                      ; if we haven't got this guid already
                      (if (= (count (filter (fn [e] (= (e "guid") guid)) (@app-state "podcasts"))) 0)
                        ; add the podcast structure to our list of new ones
                          {"guid" guid
                           "timestamp" (js/Date. (get-item-tag i :pubdate))
                           "title" (get-item-tag i :title)
                           "description" (first (.split (or (get-item-tag i :description) (get-item-tag i :itunes:summary) "") "\n"))
                           "media" (json-friendly (-> i :content (find-tag :enclosure) first :attributes))
                           "duration" (get-item-tag i :itunes:duration)
                           "source-uri" uri}))))))
            (log-error (str "Error fetching " uri)))
          ; remove the URL from our pending URLs
          (swap! syncing (fn [old] (remove (fn [x] (= x uri)) old))))))))

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

(defn component-sync-button []
  (if (> (count (@app-state "uris")) 0)
    [:button {:title "refresh" :on-click #(if (= (count @urls-syncing) 0) (sync-urls urls-syncing))}
      [:i {:class (str "fa fa-refresh" (if (> (count @urls-syncing) 0) " fa-spin spin-2x" ""))}]
      [:span {:class "url-count"} (if (> (count @urls-syncing) 0) (count @urls-syncing))]]))

(let [un (atom "") pw (atom "")]
  (defn component-user-pass [formclass message]
    [:div {:class formclass}
     [:div
       [:p message]
       [:input {:placeholder "username" :type "text" :value @un :on-change #(reset! un (-> % .-target .-value))}]
       [:input {:type "password" :value @pw :placeholder "password" :on-change #(reset! pw (-> % .-target .-value)) :on-key-down #(if (= (.-which %) 13) (submit-user-pass-form un pw))}]
       [:div {:class "buttonbar"}
         [:button {:on-click #(submit-user-pass-form un pw)} [:i {:class "fa fa-check"}]]]]]))

(defn component-auth-configured []
  [:div
    [:div {:class "buttonbar"}
      [:button {:title "home" :on-click #(redirect "#/") :id "settings-home"} [:i {:class "fa fa-arrow-circle-left"}]] 
      (component-sync-button)
      [:button {:title "scheme" :on-click #(swap! app-state toggle-scheme)} [:i {:class (str "fa " (if (= (@app-state "scheme") "night") "fa-sun-o" "fa-glass"))}]]
      [:button {:title "logout" :on-click submit-logout-request} [:i {:class "fa fa-sign-out"}]]]
    [:p [:i {:class "fa fa-check tick"}] "Successfully connected to the sync backend."]])

(defn component-setup-server-info []
  [:div [:p {:class "error"} "Could not contact the server. Please install the " [:a {:href "https://github.com/chr15m/pellet"} "pellet"] " server into a folder called 'server'." [:p "If you're using git then you should be able to run:"] [:pre [:code "git submodule init\n"  "git submodule update"] ] [:p "Or clone the repository again using:"] [:code "git clone --recursive https://github.com/chr15m/miniCast"]]])

(defn component-uri-listitem [idx item]
  [:li {:key (str "uri-listitem-" idx) :class "buttonbar"}
    [:button {:title "remove" :on-click #(swap! app-state remove-uri (item "uri"))} [:i {:class "fa fa-close"}]]
    [:div {:class "url" :type "uri"} (if (item "image-uri") [:img {:class "podcast-logo-small" :src (item "image-uri")}]) (item "uri")]])

(let [url-to-add (atom "")]
  ; adds the typed URI to the list
  (defn submit-uri []
    (swap! app-state add-uri @url-to-add)
    (reset! url-to-add ""))

  (defn component-urls-config []
    [:div
      [:div {:class "buttonbar"}
        [:button {:title "add podcast" :on-click submit-uri} [:i {:class "fa fa-check"}]]
        [:input {:placeholder "https://www.astronomycast.com/feed/" :class "url" :type "uri" :value @url-to-add :on-change #(reset! url-to-add (-> % .-target .-value)) :on-key-down #(if (= (.-which %) 13) (submit-uri))}]]
      [:ul
        (map-indexed component-uri-listitem (reverse (@app-state "uris")))]]))

(let [podcast (atom nil) podcast-parent (atom nil)]
  (defn component-podcasts []
    [:div {:class "podcasts"}
       (doall (for [p (take 100 (@app-state "podcasts"))]
         (if-let [parent (get-uri-map (p "source-uri"))]
          (let [src (get (get p "media") "url")
                duration (p "duration")
                duration-parts (if (and duration (>= (.indexOf duration ":") 0)) (.split duration ":") nil)
                duration-computed (if duration-parts (+ (* 60 60 (js/parseInt (get duration-parts 0))) (* 60 (js/parseInt (get duration-parts 1))) (js/parseInt (get duration-parts 2))) duration)
                pos (or (@play-position src) (get-in @app-state ["positions" src "value"]))
                percent (js/Math.round (* 100 (/ (js/parseFloat pos) (js/parseFloat duration-computed))))]
            [:div {:class (str "podcast_item" (if (>= percent 97) " podcast_completed")) :key (p "guid") :on-click (fn [ev] (reset! podcast p) (reset! podcast-parent parent))}
              [:div {:class "podcast_left"} [:div {:class "podcast_image"} [:img {:src (parent "image-uri")}]]]
              [:div {:class "podcast_right"}
                  (if (and pos (> pos 1) duration-computed)
                    [:div {:class "podcast_percent"} [:p {:style {"width" (str percent "%")}}]])
                [:div {:class "podcast_name"} (parent "title")]
                [:div {:class "podcast_title"} (p "title")]
                [:div {:class "podcast_description"} (p "description")]]]))))])

  (defn audio-component []
    (fn []
      (let [url ((@podcast "media") "url")]
        [:audio {:src url :id "player" :controls true}])))

  (defn do-seek [audio-element src]
    (let [last-position (or (get-in @play-position [src]) (get-in @app-state ["positions" src "value"]) 0)]
      ; (print "do-seek" audio-element src last-position)
      (if (not (= (.-currentTime audio-element) last-position))
        (do
          (print "seeking" src last-position)
          (set! (.-currentTime audio-element) last-position))
        (print "not seeking"))))

  (defn seek-when-ready [audio-element src]
      ; if we can seek now
      (if (> (.-length (.-seekable audio-element)) 0)
        (do-seek audio-element src)
        ; wait until we can seek
        (do (print "registering listener")
          (events/listenOnce audio-element "loadedmetadata" (fn [ev] (print "canplay" src) (do-seek audio-element src))))))
  
  ; function to persist where we are up to in a playing stream
  (defn store-position [now src current-time source-event]
    ;(print "store-position:" source-event now src current-time)
    ; if the updated time is different from the one we set
    (if (not (= (get-in @app-state ["positions" src "value"]) current-time))
      (swap! app-state assoc-in ["positions" src] {"timestamp" now "value" current-time})))

  (defn persist-position [audio-element event-name force-save]
    (let [src (.-src audio-element)]
            (when force-save
              (store-position (get-now) src (.-currentTime audio-element) event-name))
            ;(print "persist-position" event-name src (.-currentTime audio-element))
            (swap! play-position assoc-in [src] (.-currentTime audio-element))))
  
  (def audio-component-wrapper (with-meta audio-component
                 {:component-did-mount (fn [this old-props]
                                         (let [a (reagent/dom-node this)
                                               src (.-src a)]
                                          ; (print "component-did-mount" src last-position)
                                          ; when the player updates - keep a record in memory of where we are up to if playing
                                          (events/listen a "timeupdate" (fn [ev] (if (not (.-paused a)) (persist-position a "timeupdate" false))))
                                          ; this can happen when seek is programmatic, causing lots of trouble
                                          ;(events/listen a "seeked" (fn [ev] (persist-position a "seeked" true)))
                                          (events/listen a "pause" (fn [ev] (persist-position a "pause" true)))
                                          (events/listen a "play" (fn [ev] (persist-position a "play" true)))
                                          ; once the audio element is ready to play, set its correct position
                                          (seek-when-ready a src)))
                  :component-will-update (fn [this old-props old-children]
                                              (let [a (reagent/dom-node this)
                                                    src (.-src a)]
                                                (persist-position a "audio-component-will-update" src)))
                  :component-did-update (fn [this old-props old-children]
                                          (let [a (reagent/dom-node this)
                                                src (.-src a)]
                                            ; (print "component-did-update" src last-position)
                                            (seek-when-ready a src)))}))
  
  (defn component-podcast-playing []
    (if (and @podcast @podcast-parent)
      (let [url (get (get @podcast "media") "url")]
        [:div {:class "podcast_playing"}
          [:div {:class "podcast_playing_image"} [:img {:src (get @podcast-parent "image-uri")}]]
          [:div {:class "podcast_right"}
            [:a {:class "podcast_download" :href url} [:i {:class "fa fa-download"}]]
            [:div {:class "podcast_name"} (get @podcast-parent "title")]
            [:div {:class "podcast_title"} (get @podcast "title")]
            ; http://stackoverflow.com/a/8268563/2131094
            (if url [audio-component-wrapper] [:div {:class "error"} [:i {:class "fa fa-warning"}] "No audio found. Is this a podcast feed?"])]]))))

;; -------------------------
;; Views

(defn home-page []
  (if (case @auth-state "AUTHENTICATED" true nil true false)
    (fn []
      [:div {:class "main"}
        [:div {:class "buttonbar"}
          (component-sync-button)
          [:button {:title "settings" :on-click #(redirect "#/sync-config")} [:i {:class "fa fa-cog"}]]]
       (component-podcasts)
       (component-podcast-playing)])
    ; the user isn't logged in or hasn't set up sync - redirect to sync setup page.
    (do
      (redirect "#/sync-config")
      [:div "Redirecting to sync config."])))

(defn sync-config-page []
  (let [a @auth-state]
      [:div {:class "main"}
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
          nil)
       (component-podcast-playing)]))

(defn current-page []
  [:div {:class (@app-state "scheme")} [(session/get :current-page)]])

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

