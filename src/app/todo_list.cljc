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

(e/defn Chat-UI [username]
  (let [remewserid
        (e/server (get-in e/*http-request* [:cookies "remewserid" :value]))]
    (dom/div (dom/text "online now: ") (dom/props {:class "gcui"}))
    (dom/ul (dom/props {:class "fc"})
     (e/server
      (e/for-by first [[session-id username] present]
                (e/client
                 (dom/li (dom/props {:class "fi"}) (dom/text username))))))
    (dom/hr)
    (dom/div (dom/props {:class "newsitemlistc fc"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(newsitem-records db))] (NewsItem. id)))
    (dom/div (dom/props {:class "fi"}) (NewsItemCreate.)))
    (dom/hr)
    (dom/ul (dom/props {:class "fc"})
     (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
         (dom/li (dom/props {:class "fi"})
           (dom/strong (dom/text username))
           (dom/text " " msg)))))
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
             (dom/text pw))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text xt-id))
             )))))))

(e/defn UserCreate [] (e/client (InputSubmit. "create user"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :user/email v
    :user/id (nid)
    :user/minted-at (System/currentTimeMillis)}]]))))))))

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
#?(:cljs (def !online-user (atom "")))
(e/def online-user (e/client (e/watch !online-user)))

#?(:cljs (defn handle-response [response]
  (println response)
  (if (not= "unsuccessful" response)
    (reset! !online-user response)
    (println "Login unsuccessful just now."))))

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
  (dom/input (dom/props {:placeholder "username"})
                 (dom/on "change" (e/fn [e]
                                     (reset! login-str (.-value dom/node) ))))
  (dom/input (dom/props {:placeholder "password" :type "password"})
                 (dom/on "change" (e/fn [e]
                                     (reset! loginn-str (.-value dom/node) ))))
  (ui/button (e/fn []
                 (e/server (e/discard  (e/offload  #(xt/submit-tx !xtdb [[:xtdb.api/put
    {:xt/id (random-uuid)
    :user/id (nid)
    :user/minted-at (System/currentTimeMillis)
    :user/username ls
    :user/password (hash-with :argon2 lpw)}]])))))

      (dom/text "Create Account"))))

(e/defn Todo-list []
  (e/server
    (binding [!xtdb user/!xtdb
              db (new (db/latest-db> user/!xtdb))]

        (e/client
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
              (dom/text "NextApex"))
            (dom/div (dom/props {:class "fi"})
              (dom/text "ocean // main page"))
            (dom/div (dom/props {:class "fi"})
              (dom/text online-user))
            )
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
              (dom/text "17 people in this tribe"))
            (dom/div (dom/props {:class "fi"})
              (dom/text "current tribe: Saris and Indian Fashion"))
            (dom/div (dom/props {:class "fi"})
              (dom/text "chatroom")))
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
              ;(dom/text "chatroom-goes-here")
              (Chat-UI. "placeholder-username")))
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
          (UserCreate.)
          (dom/hr)
          

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
          (LoginPart.)
          (dom/hr)
          (CreateAccountPart.)
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
          ] ;; a vector of tribe-ids [tribe1 tribe2 tribe3]
      (e/client
        (dom/div (dom/props {:class "newsitem fi"})
          (dom/div (dom/props {:class "fr"})
            (dom/img (dom/props {:class "fi" :src (str "img/" thumbnail)}))
            (dom/div (dom/props {:class "fi"})
             (dom/text author))
            (dom/div (dom/props {:class "fi"})
             (dom/text item-id))
            (dom/div (dom/props {:class "fi"})
             (dom/text item-minted-at))
            (dom/div (dom/props {:class "fi"})
             (dom/text link))
            (dom/div (dom/props {:class "fi"})
             (dom/text title))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text xt-id)))))))))

(e/defn NewsItemCreate [] (e/client (InputSubmit. "link to add"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :item/link v
    :item/id (nid)
    :item/author "logged-in-user"
    :item/minted-at (System/currentTimeMillis)}]]))))))))

#?(:clj
   (defn newsitem-records [db]
     (->> (xt/q db '{:find [(pull ?i [:xt/id :item/minted-by :item/id :item/minted-at :item/link :item/title :item/desc])]
                     :where [[?i :item/id]]})
       (map first)
       (sort-by :item/minted-at)
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
             (dom/text title))
            (dom/div (dom/props {:class "fi"})
             (dom/text desc))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text xt-id)))))))))

(e/defn TribeCreate [] (e/client (InputSubmit. "new tribe name"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :tribe/title v
    :tribe/id (nid)
    :tribe/author "logged-in-user"
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
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text xt-id)))))))))

(e/defn FeedbackCreate [] (e/client (InputSubmit. "feedback desc"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :feedback/desc v
    :feedback/id (nid)
    :feedback/author "logged-in-user"
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
             (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text xt-id)))))))))

(e/defn FeatureRequestCreate [] (e/client (InputSubmit. "Feature Request"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :feature-request/desc v
    :feature-request/id (nid)
    :feature-request/author "logged-in-user"
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

