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
                                  ;;(add-event-notif :new-global-chat-msg (. System (currentTimeMillis)))
                                  )
                                 (set! (.-value dom/node) "")))))))))

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
          username (:user/username e)
          user-id (:user/id e)
          user-email (:user/email e)
          user-phone (:user/phone e)
          user-minted-at (:user/minted-at e)
          user-minted-by (:user/minted-by e)
          user-octave (:user/octave e)
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
             (dom/text user-tribes)))
          (dom/text user-octave))))))

(e/defn UserCreate [] (e/client (InputSubmit. "create user"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
  [[:xtdb.api/put
    {:xt/id (random-uuid)
    :user/email v
    :user/id (nid)}]]))))))))

#?(:clj
   (defn user-records [db]
     (->> (xt/q db '{:find [(pull ?u [:xt/id :user/username :user/id :user/email :user/phone :user/minted-at :user/minted-by :user/octave :user/tribes])]
                     :where [[?u :user/id]]})
       (map first)
       (sort-by :user/minted-at)
       vec)))

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
                 
              ; (e/server 
              ;  (e/discard
              ;    (e/offload
              ;      #(xt/submit-tx !xtdb [[:xtdb.api/put
              ;                              {:xt/id (nid)
              ;                              :user/number (inc user-count)
              ;                              :user/username (e/watch login-str)
              ;                              :user/password (hash-with :argon2 (e/watch loginn-str))
              ;                              :task/status :active}]]))))
              ))
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
            (dom/text "login/create an account")))
        (dom/div (dom/props {:class "fr"})
          (dom/div (dom/props {:class "fi"})
            (dom/text "17 people in this tribe"))
          (dom/div (dom/props {:class "fi"})
            (dom/text "current tribe: Saris and Indian Fashion"))
          (dom/div (dom/props {:class "fi"})
            (dom/text "chatroom")))
        (dom/div (dom/props {:class "fr"})
          (dom/div (dom/props {:class "fi"})
            (dom/text "user avatars"))
          (dom/div (dom/props {:class "fi"})
            (dom/text "newsitems"))
          (dom/div (dom/props {:class "fi"})
            (dom/text "chatroom")))
        (dom/h1 (dom/text "welcome to NextApex.co"))
        (dom/p (dom/text "realtime link share"))
        (dom/hr)
        (dom/div (dom/props {:class "userlistc fc"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(user-records db))] (UserItem. id))))
        (UserCreate.)
        (dom/hr)
        (dom/div (dom/props {:class "newsitemlistc fc"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(newsitem-records db))] (NewsItem. id))))
        (NewsItemCreate.)
        (dom/hr)
        (dom/div (dom/props {:class "tribelistc fc"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(tribe-records db))] (TribeItem. id))))
        (TribeCreate.)

        (dom/hr)
        (Chat-UI. "placeholder-username")
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
          ] ;; a vector of tribe-ids [tribe1 tribe2 tribe3]
      (e/client
        (dom/div (dom/props {:class "newsitem fi"})
          (dom/div (dom/props {:class "fr"})
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
             (dom/text desc))))))))

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
             (dom/text desc))))))))

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









;;userList [oo   ]
;;itemsList [oo ]
;;tribesList [ ]
;;feedbackList [ ]
;;featureList [ ]

;;createAccount [oo    ]
;;login [o  ]
;;

;;oceanDisplay [ | | ]

;;tribeDisplay [ | | ]

;;tribe [curators: , observers: , topic, id#, membercount, max-member-count]

;;

