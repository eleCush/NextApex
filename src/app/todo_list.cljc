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
  (subs (str (random-uuid)) 4 13))

(e/def !xtdb)
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (e/watch !msgs))))

#?(:clj (defonce !present (atom {}))) ; session-id -> user
(e/def present (e/server (e/watch !present)))

#?(:clj (defonce !eventses (atom []))) ; eventses are coll of event-type & timestamp and only accepts timestamps within :big-window: while interface displays :lil-window: worth of notifs
(e/def eventses (e/server (e/watch !eventses)))

(e/defn Link-and-Chat-UI [username]
  (let [usernamefromhttpreq
        (e/server (get-in e/*http-request* [:cookies "username" :value]))]
    (dom/div (dom/text "online now: ") (dom/props {:class "gcui"}))
    (dom/ul (dom/props {:class "fc"})
     (e/server
      (e/for-by first [[session-id username] present]
                (e/client
                 (dom/li (dom/props {:class "fi"}) (dom/text username))))))
    (dom/hr)
    (dom/div (dom/props {:class "newsitemlistc fc"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(newsitem-records db))] (NewsItem. id)))
    (dom/div (dom/props {:class "fing"}) (when (not= "" online-user) (NewsItemCreate.))))
    (dom/hr)
    (dom/ul (dom/props {:class "fc"})
     (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
         (dom/li (dom/props {:class "fing"})
           (dom/strong (dom/text username ": "))
           (dom/text " " msg)))))
      (when (not= "" online-user)
      (do
        (dom/input
         (dom/props {:placeholder "Kind a message [global chat]" :class "kind fi"})
         (dom/on "keydown" (e/fn [e]
                             (when (= "Enter" (.-key e))
                               (when-some [v (empty->nil (-> e .-target .-value))]
                                 (dom/style {:background-color "yellow"}) ; loading
                                 (e/server
                                  (swap! !msgs #(cons {::username username ::msg v}
                                                      (take 9 %)))
                                  ;;(add-event-notif :new-global-chat-msg (. System (currentTimeMillis)))
                                  )
                                 (set! (.-value dom/node) "")))))))))))

(e/defn Link-and-Chat-Extended []
  (e/client
   (let [session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"]))]
     (if-not (some? online-user)
       (Link-and-Chat-UI. (str "Fresh Presence"))
       (do
         (e/server
          (swap! !present assoc session-id online-user)
          (e/on-unmount #(swap! !present dissoc session-id)))
         ;(dom/div (dom/text "Authenticated as: " remewserid))
         (Link-and-Chat-UI. online-user))))))

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

(e/defn UserItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id (:xt/id e)
          username (:user/username e)
          user-id (:user/id e)
          user-email (:user/email e)
          user-phone (:user/phone e)
          user-minted-at (:user/minted-at e)
          user-minted-by (:user/minted-by e)
          user-octave (:user/octave e)
          pw (:user/password e)
          user-tribes (:user/tribes e)] ;; a vector of tribe-ids [tribe1 tribe2 tribe3]
      (e/client
        (dom/div (dom/props {:class "useritem fi"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (dom/text username))
            (dom/div (dom/props {:class "fi"})
             (dom/text user-id))
            (dom/div (dom/props {:class "fi b"})
             (dom/text user-email))
            (dom/div (dom/props {:class "fi"})
             (dom/text user-phone))
            (dom/div (dom/props {:class "fi"})
             (dom/text user-minted-at))
            (dom/div (dom/props {:class "fi"})
             (dom/text (subs pw 21 32)))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗"))
             )))))))

#?(:clj
   (defn user-records [db]
     (->> (xt/q db '{:find [(pull ?u [:xt/id :user/username :user/id :user/email :user/phone :user/minted-at :user/minted-by :user/octave :user/tribes :user/password])]
                     :where [[?u :user/id]]})
       (map first)
       (sort-by :user/minted-at)
       vec)))

;;move between membranes with e/def (creates a new thing in the third layer accessible to both)
#?(:cljs (def login-str (atom "")))
(e/def ls (e/client (e/watch login-str)))
#?(:cljs (def loginn-str (atom "")))
(e/def lpw (e/client (e/watch loginn-str)))
#?(:cljs (def !ca-usrname (atom "")))
(e/def ca-usrname (e/client (e/watch !ca-usrname)))
#?(:cljs (def !ca-password (atom "")))
(e/def ca-password (e/client (e/watch !ca-password)))
#?(:cljs (def !online-user (atom "")))
(e/def online-user (e/client (e/watch !online-user)))
#?(:cljs (def !create-account-visible (atom false)))
(e/def create-account-visible (e/client (e/watch !create-account-visible)))
#?(:cljs (def !create-account-msg (atom "")))
(e/def create-account-msg (e/client (e/watch !create-account-msg)))
#?(:cljs (def !view (atom :main)))
(e/def view (e/client (e/watch !view)))
#?(:cljs (def !current-tribe-view (atom "ocean")))
(e/def current-tribe-view (e/client (e/watch !current-tribe-view)))


#?(:cljs (defn handle-response [response]
  (println response)
  (if (not= "unsuccessful" response)
    (reset! !online-user response)
    (println "Login unsuccessful just now."))))

#?(:cljs (defn handle-logout-response [response]
  (println response)
  (if (not= "unsuccessful" response)
    (reset! !online-user "")
    (println "Logoutn unsuccessful just now."))))

#?(:cljs (defn clj->json [clj-data]
  (js/JSON.stringify (clj->js clj-data))))

(e/defn LoginPart []
 (e/client
  (dom/input (dom/props {:placeholder "username"})
                 (dom/on "change" (e/fn [e]
                                     (reset! login-str (.-value dom/node) ))))
  (dom/input (dom/props {:placeholder "password" :type "password"})
                 (dom/on "change" (e/fn [e]
                                     (reset! loginn-str (.-value dom/node) ))))
  (ui/button (e/fn []
                (POST "http://localhost:8080/nextapex-login" 
                   {:params {:user ls :pass lpw}
                    :format :raw
                   :handler handle-response}))
               (dom/text "Click to Login"))))

(e/defn CreateAccountPart []
 (e/client
  (if create-account-visible 
   (do
  (dom/input (dom/props {:placeholder "username"})
                 (dom/on "change" (e/fn [e]
                                     (reset! !ca-usrname (.-value dom/node) ))))
  (dom/input (dom/props {:placeholder "password" :type "password"})
                 (dom/on "change" (e/fn [e]
                                     (reset! !ca-password (.-value dom/node) ))))
  (ui/button (e/fn []
                 (e/client (reset! !create-account-msg "Account created"))
                 (e/server (e/discard  (e/offload  #(xt/submit-tx !xtdb [[:xtdb.api/put
    {:xt/id (random-uuid)
    :user/id (nid)
    :user/minted-at (System/currentTimeMillis)
    :user/username ca-usrname
    :user/password (hash-with :argon2 ca-password)}]])))))

      (dom/text "Create Account"))
      (ui/button (e/fn [] (reset! !create-account-visible false)) (dom/text "✗")))
      ;
      ;else
      (ui/button (e/fn [] (reset! !create-account-visible true)) (dom/text "create an account...")))
      ))

(e/defn Todo-list []
  (e/server (let [username-from-http-req (e/server (get-in e/*http-request* [:cookies "username" :value]))]
    (binding [!xtdb user/!xtdb
              db (new (db/latest-db> user/!xtdb))]
        (e/client
          (if (some? username-from-http-req) (reset! !online-user username-from-http-req)) ;;check session, set cljs var
          (dom/div (dom/props {:class "bigc"})
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                (dom/text "NextApex"))
              (dom/div (dom/props {:class "fi"})
                (dom/text "ocean // main page"))
              (dom/div (dom/props {:class "fi"})
                (dom/text online-user))
                (if (not (empty? online-user))
                  (ui/button (e/fn []
                    (POST "http://localhost:8080/logout" 
                      {:params {:hey "log me out"}
                       :format :raw
                       :handler handle-logout-response}))
                    (dom/props {:class "ra"})
                    (dom/text "Logout"))
                  (LoginPart.))
              )
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                (dom/text "17 people in this tribe"))
              (dom/div (dom/props {:class "fi"})
                (when (= :main view)
                  (dom/text "Current Tribe: " current-tribe-view)
                  (ui/button (e/fn [] (reset! !view :tribes)) (dom/text "To Tribes List")))
                (when (= :tribes view)
                  (dom/text "Ocean")))
              (dom/div (dom/props {:class "fi"})
                (dom/text current-tribe-view " chatroom")))
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                ;(dom/text "chatroom-goes-here")
                (when (= :tribes view)
                  (dom/div (dom/props {:class "tribelistc fc"})
                    (ui/button (e/fn [] (reset! !view :main) (reset! !current-tribe-view "ocean")) (dom/text "To Ocean"))
                    (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(tribe-records db))] (TribeItem. id)))))
                (when (= :main view)
                  (Link-and-Chat-Extended. "placeholder-username"))))
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                (dom/text "currently 0 free slots in this tribe"))
              (dom/div (dom/props {:class "fi"})
                (dom/text "join waitlist (5 people so far)")))
            (dom/h1 (dom/text "welcome to NextApex.co"))
            (dom/p (dom/text "realtime link share"))
            (dom/hr)
            (dom/div (dom/props {:class "userlistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(user-records db))] (UserItem. id))))
            

            (dom/hr)
            (dom/div (dom/props {:class "tribelistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(tribe-records db))] (TribeItem. id))))
            (TribeCreate.)
            (dom/hr)
            (dom/div (dom/props {:class "feedbacklistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(feedback-records db))] (FeedbackItem. id))))
            (FeedbackCreate.)
            (dom/hr)
            (dom/div (dom/props {:class "featurerequestlistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(feature-request-records db))] (FeatureRequestItem. id))))
            (FeatureRequestCreate.)

            (dom/hr)
            
            (dom/hr)

              (dom/hr)
              (if (not= "" create-account-msg) 
               (dom/div (dom/text create-account-msg))
               (if (empty? online-user)
                 (CreateAccountPart.)))
              (dom/hr)

        )
      )
    )
  )
 )
)

(e/defn NewsItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          author (:item/minted-by e)
          item-id (:item/id e)
          item-minted-at (:item/minted-at e)
          link (:item/link e)
          title (:item/title e) ;;hn style is (xor link desc)
          desc (:item/desc e)
          thumbnail (:item/thumbnail e)
          upvotes (or (:item/upvotes e) 0)
          time-since-minted (- e/system-time-ms item-minted-at)
          hrs-since-minted (/ time-since-minted 3600)
          gravity 1.5
          score (/ upvotes (Math/pow (+ hrs-since-minted 2) gravity))
          ] ;; a vector of tribe-ids [tribe1 tribe2 tribe3]
      (e/client
        (dom/div (dom/props {:class "newsitem fing"})
          (dom/div (dom/props {:class "fr"})
            (dom/img (dom/props {:class "fi" :src (str "img/" thumbnail)}))
            (ui/button (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
              [[:xtdb.api/put
              {:xt/id xt-id
              :item/link link
              :item/title title
              :item/desc desc
              :item/thumbnail thumbnail
              :item/id item-id
              :item/minted-by author
              :item/upvotes (inc upvotes)
              :item/score score
              :item/minted-at (System/currentTimeMillis)}]]))))) (dom/text "+"))
            (dom/div (dom/props {:class "fi"})
             (dom/text link))
            (dom/div (dom/props {:class "fi"})
             (dom/text author))
            (dom/div (dom/props {:class "fi"})
             (dom/text item-id))
            (dom/div (dom/props {:class "fi"})
             (dom/text item-minted-at))
            
            (dom/div (dom/props {:class "fi"})
             (dom/text title))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (dom/text time-since-minted))
            (dom/div (dom/props {:class "fi"})
             (dom/text (.toFixed score 3)))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn NewsItemCreate [] (e/client (InputSubmit. "link to add"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :item/link v
    :item/id (nid)
    :item/minted-by online-user
    :item/score 0
    :item/minted-at (System/currentTimeMillis)}]]))))))))

#?(:clj
   (defn newsitem-records [db]
     (->> (xt/q db '{:find [(pull ?i [:xt/id :item/minted-by :item/id :item/minted-at :item/link :item/title :item/desc :item/score])]
                     :where [[?i :item/id]]})
       (map first)
       ;(sort-by :item/minted-at)
       (sort-by :item/score >)
       vec)))

(e/defn TribeItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          creator (:tribe/minted-by e)
          tribe-id (:tribe/id e)
          tribe-minted-at (:tribe/minted-at e)
          link (:tribe/link e)
          title (:tribe/title e) ;;hn style is (xor link desc)
          desc (:tribe/desc e)
          ] ;; a vector of tribe-ids [tribe1 tribe2 tribe3]
      (e/client
        (dom/div (dom/props {:class "newsitem fi"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (dom/text creator))
            (dom/div (dom/props {:class "fi"})
             (dom/text tribe-id))
            (dom/div (dom/props {:class "fi"})
             (dom/text tribe-minted-at))
            (dom/div (dom/props {:class "fi"})
             (dom/text link))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [] (reset! !current-tribe-view title) (reset! !view :main)) (dom/text title)))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn TribeCreate [] (e/client (InputSubmit. "new tribe name"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :tribe/title v
    :tribe/id (nid)
    :tribe/minted-by online-user
    :tribe/minted-at (System/currentTimeMillis)}]]))))))))

#?(:clj
   (defn tribe-records [db]
     (->> (xt/q db '{:find [(pull ?t [:xt/id :tribe/minted-by :tribe/id :tribe/minted-at :tribe/title :tribe/members])]
                     :where [[?t :tribe/id]]})
       (map first)
       (sort-by :tribe/minted-at)
       vec)))

(e/defn FeedbackItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          minted-by (:feedback/minted-by e)
          feedback-minted-at (:feedback/minted-at e)
          link (:feedback/link e)
          mood (:feedback/mood e)
          desc (:feedback/desc e)
          ] 
      (e/client
        (dom/div (dom/props {:class "newsitem fi"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (dom/text mood))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (dom/text feedback-minted-at))
            (dom/div (dom/props {:class "fi"})
             (dom/text minted-by))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn FeedbackCreate [] (e/client (InputSubmit. "feedback desc"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :feedback/desc v
    :feedback/id (nid)
    :feedback/minted-by online-user
    :feedback/mood "current-mood"
    :feedback/minted-at (System/currentTimeMillis)}]]))))))))

#?(:clj
   (defn feedback-records [db]
     (->> (xt/q db '{:find [(pull ?fb [:xt/id :feedback/desc :feedback/minted-by :feedback/id :feedback/minted-at])]
                     :where [[?fb :feedback/id]]})
       (map first)
       (sort-by :feedback/minted-at)
       vec)))

(e/defn FeatureRequestItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          minted-by (:feedback/minted-by e)
          feature-request-minted-at (:feature-request/minted-at e)
          link (:feature-request/link e)
          mood (:feature-request/mood e)
          desc (:feature-request/desc e)
          ] 
      (e/client
        (dom/div (dom/props {:class "newsitem fi"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (dom/text mood))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (dom/text feature-request-minted-at))
            (dom/div (dom/props {:class "fi"})
             (dom/text minted-by))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn FeatureRequestCreate [] (e/client (InputSubmit. "Feature Request"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :feature-request/desc v
    :feature-request/id (nid)
    :feature-request/minted-by online-user
    :feature-request/mood "current-mood"
    :feature-request/minted-at (System/currentTimeMillis)}]]))))))))

#?(:clj
   (defn feature-request-records [db]
     (->> (xt/q db '{:find [(pull ?frq [:xt/id :feature-request/desc :feature-request/minted-by :feature-request/id :feature-request/minted-at])]
                     :where [[?frq :feature-request/id]]})
       (map first)
       (sort-by :feature-request/minted-at)
       vec)))




;;userList [ooo       ]
;;itemsList [ooooo    ]
;;tribesList [ooooo   ]
;;feedbackList [ooooo ]
;;featureList [o      ]

;;createAccount [oo   ]
;;login [oo           ]
;;

;;oceanDisplay [ | | ]

;;tribeDisplay [ | | ]

;;tribe [curators: , observers: , topic, id#, membercount, max-member-count]

;;

