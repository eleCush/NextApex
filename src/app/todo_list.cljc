(ns app.todo-list
  (:require #?(:clj [app.xtdb-contrib :as db])
            [cljs.js :as js]
            [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:clj [cryptohash-clj.api :as ch :refer [hash-with verify-with]])
            #?(:cljs [cljs.core :refer [js->clj clj->js]])
            #?(:cljs [ajax.core :as ajax :refer [PUT POST]])
            #?(:clj [clj-http.client :as httpclient])
            #?(:clj [postal.core :as postal])
            [xtdb.api #?(:clj :as :cljs :as-alias) xt]))

#?(:clj
   (def mail-config
     {:host "smtp-relay.sendinblue.com"
      :port 587
      :user "v1nc3ntpull1ng@gmail.com"
      :pass "CcEAZqTdVrDfRPzS"
      :tls true}))

#?(:cljs (defn set-scroll-position [x y] (.scrollTo js/window x y)))
#?(:cljs (defn open-url [url] (.open js/window url)))

(defn nid
  "Mints a new nid."
  []
  (subs (str (random-uuid)) 0 9))

(e/def !xtdb)
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (e/watch !msgs))))

#?(:clj (defonce !present (atom {}))) ; session-id -> user
(e/def present (e/server (e/watch !present)))

#?(:clj (defonce !eventses (atom []))) ; eventses are coll of event-type & timestamp and only accepts timestamps within :big-window: while interface displays :lil-window: worth of notifs
(e/def eventses (e/server (e/watch !eventses)))

(e/defn Chat-UI [username]
  (let [remewserid
        (e/server (get-in e/*http-request* [:cookies "remewserid" :value]))]
    (dom/div (dom/text "online now: ") (dom/props {:class "gcui"}))
    (dom/ul
     (e/server
      (e/for-by first [[session-id username] present]
                (e/client
                 (dom/li (dom/text username))))))
    (dom/hr)
    (dom/ul
     (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
         (dom/li (dom/strong (dom/text username))
                 (dom/text " " msg))))))
    (if-not (some? remewserid)
      (dom/div
       (dom/text "Login to chat."))
      (do
        (dom/input
         (dom/props {:placeholder "Kind a message [global chat]" :class "kind"})
         (dom/on "keydown" (e/fn [e]
                             (when (= "Enter" (.-key e))
                               (when-some [v (empty->nil (-> e .-target .-value))]
                                 (dom/style {:background-color "yellow"}) ; loading
                                 (e/server
                                  (swap! !msgs #(cons {::username username ::msg v}
                                                      (take 9 %)))
                                  (add-event-notif :new-global-chat-msg (. System (currentTimeMillis))))
                                 (set! (.-value dom/node) ""))))))))))

(e/defn ChatExtended []
  (e/client
   (let [session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"]))
         current-remewser (e/server (get-in e/*http-request* [:cookies "remewserid" :value]))
         usernomen  "usernomenk"
         rec (e/server  (e/offload #(the-mine-user-record db (str current-remewser))))
         r (first rec)
         yuzaneim (or (:user/username r) (str "Guest of " (count present)))]
     (if-not (some? usernomen)
       (Chat-UI. (str "Fresh Music Lover"))
       (do
         (e/server
          (swap! !present assoc session-id yuzaneim)
          (e/on-unmount #(swap! !present dissoc session-id)))
         ;(dom/div (dom/text "Authenticated as: " remewserid))
         (Chat-UI. yuzaneim))))))

(e/defn InputSubmit [ph F]
  ; Custom input control using lower dom interface for Enter handling
  (e/server
   ;(when (not (empty? (get-in e/*http-request* [:cookies "remewserid" :value])))
     (e/client
      (dom/input (dom/props {:placeholder (or ph "")})
                 (dom/on "keydown" (e/fn [e]
                                     (when (= "Enter" (.-key e))
                                       (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                         (new F v)
                                         (set! (.-value dom/node) "")))))))));)

(e/defn AdminInputSubmit [ph F]
  ; Custom input control using lower dom interface for Enter handling
     (e/client
      (if (= "v1nc3ntpull1ng@gmail.com" (e/server (get-in e/*http-request* [:cookies "useremail" :value])))
        (dom/input (dom/props {:placeholder (or ph "")})
                  (dom/on "keydown" (e/fn [e]
                                      (when (= "Enter" (.-key e))
                                        (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                          (new F v)
                                          (set! (.-value dom/node) "")))))))))

(e/defn TodoItem [id]
  (e/server
    (let [e (xt/entity db id)
          status (:task/status e)]
      (e/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (e/discard
                  (e/offload
                    #(xt/submit-tx !xtdb [[:xtdb.api/put
                                           {:xt/id id
                                            :task/description (:task/description e) ; repeat
                                            :task/status (if v :done :active)}]])))))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn TodoCreate []
  (e/client
    (InputSubmit. "create todo" 
                  (e/fn [v]
                    (e/server
                      (e/discard
                        (e/offload
                          #(xt/submit-tx !xtdb [[:xtdb.api/put
                                                 {:xt/id (random-uuid)
                                                  :task/description v
                                                  :task/status :active}]]))))))))

#?(:clj
   (defn todo-records [db]
     (->> (xt/q db '{:find [(pull ?e [:xt/id :task/description])]
                     :where [[?e :task/status]]})
       (map first)
       (sort-by :task/description)
       vec)))

(comment (todo-records user/db))

#?(:clj
   (defn todo-count [db]
     (count (xt/q db '{:find [?e] :in [$ ?status]
                       :where [[?e :task/status ?status]]}
              :active))))

(def login-str (atom ""))
(def loginn-str (atom ""))

(e/defn LoginPart []
 (e/client
  (dom/input (dom/props {:placeholder "username"})
                 (dom/on "change" (e/fn [e]
                                     (reset! login-str (.-value dom/node) ))))
  (dom/input (dom/props {:placeholder "password" :type "password"})
                 (dom/on "change" (e/fn [e]
                                     (reset! loginn-str (.-value dom/node) ))))
  (ui/button (e/fn [v]
              (e/server
                (e/client (.log js/console (e/watch login-str) " | " (e/watch loginn-str)))
              
              ;; ajax call here for login

              ;  (e/discard
              ;    (e/offload
              ;      #(xt/submit-tx !xtdb [[:xtdb.api/put
              ;                              {:xt/id (random-uuid)
              ;                              :task/description v
              ;                              :task/status :active}]])))
              ))
               (dom/text "Click to Login"))))

(e/defn CreateAccountPart []
 (e/client
  (dom/input (dom/props {:placeholder "username"})
                 (dom/on "change" (e/fn [e]
                                     (reset! login-str (.-value dom/node) ))))
  (dom/input (dom/props {:placeholder "password" :type "password"})
                 (dom/on "change" (e/fn [e]
                                     (reset! loginn-str (.-value dom/node) ))))
  (ui/button (e/fn [v]
              (e/server
                (e/client (.log js/console (e/watch login-str)))
              
              ;  (e/discard
              ;    (e/offload
              ;      #(xt/submit-tx !xtdb [[:xtdb.api/put
              ;                              {:xt/id (random-uuid)
              ;                              :task/description v
              ;                              :task/status :active}]])))
              ))
               (dom/text "Create Account"))))

(e/defn Todo-list []
  (e/server
    (binding [!xtdb user/!xtdb
              db (new (db/latest-db> user/!xtdb))]
      (e/client
        (dom/link (dom/props {:rel :stylesheet :href "/todo-list.css"}))
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (dom/div (dom/props {:class "todo-list"})
          (TodoCreate.)
          (dom/div {:class "todo-items"}
            (e/server
              (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(todo-records db))]
                (TodoItem. id))))
          (Chat-UI.)
          (dom/hr)
          (LoginPart.)
          (dom/hr)
          (CreateAccountPart.)
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"})
              (dom/text (e/server (e/offload #(todo-count db)))))
            (dom/text " items left")))))))

;;userList
;;itemsList
;;tribesList
;;feedbackList
;;featureList

;;createAccount [o]
;;login [o]
;;




