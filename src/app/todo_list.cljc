(ns app.todo-list
  (:require #?(:clj [app.xtdb-contrib :as db])
            [cljs.js :as js]
            [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:clj [cryptohash-clj.api :as ch :refer [hash-with verify-with]])
            #?(:clj [clj-time.core :as time])
            #?(:cljs [cljs.core :refer [js->clj clj->js]])
            #?(:cljs [ajax.core :as ajax :refer [PUT POST]])
            #?(:clj [clj-http.client :as httpclient])
            #?(:clj [postal.core :as postal])
            [xtdb.api #?(:clj :as :cljs :as-alias) xt]))

#?(:cljs (defn set-scroll-position [x y] (.scrollTo js/window x y)))
#?(:cljs (defn open-url [url] (.open js/window url)))
#?(:cljs (defn get-current-path [] (.-pathname js/location)))
#?(:cljs (defn replace-state [state title url] (.replaceState js/history state title url)))
#?(:cljs (defn update-url [url] (replace-state nil "" url)))

(defn nid "Mints a new nid." [] (subs (str (random-uuid)) 4 13))

(e/def !xtdb)
(e/def db) ; injected database ref; Electric defs are always dynamic
#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (e/watch !msgs))))
#?(:clj (defonce !present (atom {}))) ; session-id -> user
(e/def present (e/server (e/watch !present)))
#?(:clj (defonce !eventses (atom []))) ; eventses are coll of event-type & timestamp and only accepts timestamps within :big-window: while interface displays :lil-window: worth of notifs
(e/def eventses (e/server (e/watch !eventses)))

(e/defn Link-and-Chat-UI [username]
    (dom/div (dom/props {:class "newsitemlistc fc"})
     (.log js/console "current tribe id = " current-tribe-id)
        (e/server (let [cti (if (empty? current-tribe-id) "global"  current-tribe-id)]
          (e/for-by :xt/id [{:keys [xt/id rank]} (e/offload #(newsitem-records db cti))] (NewsItem. id rank))))
        (when (not= "" online-user) (NewsItemCreate.)))
    (dom/ul (dom/props {:class "fc w croom"})
     (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
         (dom/li (dom/props {:class "fing"})
           (dom/strong (dom/text username ": "))
           (dom/text " " msg)))))
      (when (not= "" online-user)
      (do
        (dom/input
         (dom/props {:placeholder "Kind a message [global chat]" :class "kind"})
         (dom/on "keydown" (e/fn [e]
                             (when (= "Enter" (.-key e))
                               (when-some [v (empty->nil (-> e .-target .-value))]
                                 (dom/style {:background-color "yellow"}) ; loading
                                 (e/server
                                   (let [u  (get-in e/*http-request* [:cookies "username" :value])
                                         lm (get-in e/*http-request* [:cookies "loginmoment" :value])
                                         uh (get-in e/*http-request* [:cookies "userhash" :value])
                                         vw (verify-with :argon2 (str lm u) uh)]
                                     (when vw (swap! !msgs #(cons {::username username ::msg v}
                                                         (take 9 %)))))
                                  ;;(add-event-notif :new-global-chat-msg (. System (currentTimeMillis)))
                                  )
                                 (set! (.-value dom/node) "")))))))))
      (dom/ul (dom/props {:class "fc w"})
      (dom/div (dom/text "Online: ") (dom/props {:class "gcui"}))
      (e/server
        (e/for-by first [[session-id username] present]
                  (e/client
                  (dom/li (dom/props {:class "fi"}) (dom/span (dom/props {:class "g"}) (dom/text "•")) (dom/text username)))))))

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
            (dom/div (dom/props {:class "fi www"})
             (dom/text username))
            (dom/div (dom/props {:class "fi"})
             (dom/text user-id))
            (dom/div (dom/props {:class "fi b"})
             (dom/text user-email))
            (dom/div (dom/props {:class "fi w"})
             (dom/text user-phone))
            (dom/div (dom/props {:class "fi ww"})
             (dom/text user-minted-at))
            (dom/div (dom/props {:class "fi www"})
             (dom/text (subs pw 21 32)))
            (dom/div (dom/props {:class "fi w"})
             (when (= "R" online-user)
              (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))
             )))))))

#?(:clj
   (defn user-records [db]
     (->> (xt/q db '{:find [(pull ?u [:xt/id :user/username :user/id :user/email :user/phone :user/minted-at :user/minted-by :user/octave :user/tribes :user/password])]
                     :where [[?u :user/id]]})
       (map first)
       (sort-by :user/minted-at)
       vec)))
#?(:clj (defn ensure-http-prefix [url] (if (or (.startsWith url "http://") (.startsWith url "https://")) url (str "https://" url))))

;;move between membranes with e/def (creates a new thing in the third layer accessible to both)
#?(:cljs (def login-str (atom "")))
(e/def ls (e/client (e/watch login-str)))
#?(:cljs (def loginn-str (atom "")))
(e/def lpw (e/client (e/watch loginn-str)))
#?(:cljs (def !ca-usrname (atom "")))
(e/def ca-usrname (e/client (e/watch !ca-usrname)))
#?(:cljs (def !ca-usr-email (atom "")))
(e/def ca-usr-email (e/client (e/watch !ca-usr-email)))
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
#?(:cljs (def !current-tribe-id (atom "global")))
(e/def current-tribe-id (e/client (e/watch !current-tribe-id)))
#?(:cljs (def !current-tribe-title (atom "global")))
(e/def current-tribe-title (e/client (e/watch !current-tribe-title)))
#?(:cljs (def !current-item-id (atom "")))
(e/def current-item-id (e/client (e/watch !current-item-id)))
#?(:cljs (def !current-item-xt-id (atom "")))
(e/def current-item-xt-id (e/client (e/watch !current-item-xt-id)))
#?(:cljs (def !parent-reply-xt-id (atom [])))
(e/def parent-reply-xt-id (e/client (e/watch !parent-reply-xt-id)))


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
                (POST "https://nextapex.co/nextapex-login"
                   {:params {:user ls :pass lpw}
                    :format :raw
                   :handler handle-response}))
               (dom/text "Click to Login"))))

(e/defn CreateAccountPart []
 (e/client
  (if create-account-visible
   (let [valid-email? (fn [email] (re-matches  #"(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)" email))]
    (dom/input (dom/props {:placeholder "username"})
                  (dom/on "change" (e/fn [e]
                                      (reset! !ca-usrname (.-value dom/node) ))))
    (dom/input (dom/props {:placeholder "email"})
                  (dom/on "change" (e/fn [e]
                                      (reset! !ca-usr-email (.-value dom/node) ))))
    (dom/input (dom/props {:placeholder "password" :type "password"})
                  (dom/on "change" (e/fn [e]
                                      (reset! !ca-password (.-value dom/node) ))))
    (ui/button (e/fn []
                (if (and (not (empty? ca-password)) (valid-email? ca-usr-email) (<= 2 (count ca-usrname)))
                  (do
                  (e/client (reset! !create-account-msg "Creating account..."))
                  (e/server (e/discard  (e/offload  #(xt/submit-tx !xtdb [[:xtdb.api/put
                    {:xt/id (random-uuid)
                    :user/id (nid)
                    :user/minted-at (System/currentTimeMillis)
                    :user/username ca-usrname
                    :user/email ca-usr-email
                    :user/password (str (hash-with :argon2 ca-password))}]]))))
                  (e/client (reset! !create-account-msg "Account created.")))
        (e/client (reset! !create-account-msg "Must be 2 chars+, valid email, and have a password.")))
      )

        (dom/text "Create Account"))
        (ui/button (e/fn [] (reset! !create-account-visible false)) (dom/text "✗")))
        ;
        ;else
      (ui/button (e/fn [] (reset! !create-account-visible true)) (dom/text "create an account...")))))

(e/defn Todo-list []
  (e/server (let [u (e/server (get-in e/*http-request* [:cookies "username" :value]))
                  lm (e/server (get-in e/*http-request* [:cookies "loginmoment" :value]))
                  uh (e/server (get-in e/*http-request* [:cookies "userhash" :value]))]
    (binding [!xtdb user/!xtdb
              db (new (db/latest-db> user/!xtdb))]
        (e/client
          (when (not (empty? uh)) (if (e/server (verify-with :argon2 (str lm u) uh)) (do (reset! !online-user u) (reset! !online-user "")))) ;;check session, set cljs var
          (let [sta (rest (clojure.string/split (get-current-path) "/" ))]
            (when (= 3 (count sta))
              (reset! !view :main)
              (reset! !current-tribe-id (second sta))
              (reset! !current-item-id (last sta))
              (reset! !current-item-xt-id (last sta))
              (reset! !current-tribe-title (e/server (e/offload #(get-tribe-title-from-id db current-tribe-id))))
              )
            (when (= 1 (count sta))
              (if (= "tribes" (first sta)) (reset! !view :tribes))
              (reset! !current-tribe-id "")
              (reset! !current-item-id "")
              (reset! !current-item-xt-id "")
              (reset! !current-tribe-title "")
              )
            (println "sta: " sta)
            (println (first sta))
            (println (second sta))
            (println (last sta))
            (println current-tribe-title))
          (dom/div (dom/props {:class "bigc"})
            (dom/div (dom/props {:class "fr hdr"})
              (dom/div (dom/props {:class "fi w"})
                (dom/text "NextApex"))
              (dom/div (dom/props {:class "fi w"})
                (ui/button (e/fn [] (reset! !view :main) (update-url "/global/") (reset! !current-item-id "") (reset! !current-item-xt-id "") (reset! !current-tribe-id "global") (reset! !current-tribe-title "global")) (dom/text "global") (dom/props {:class (if (= view "global") "selegold" "")})))
              (dom/div (dom/props {:class "fi"})
                (ui/button (e/fn [] (update-url "/tribes/") (reset! !current-item-id "") (reset! !current-item-xt-id "") (reset! !view :tribes)) (dom/text "tribes")))
              (dom/div (dom/props {:class "fi"})
                (dom/text online-user))
              (if (not (empty? online-user))
                  (ui/button (e/fn []
                    (POST "https://nextapex.co/logout"
                      {:params {:hey "log me out"}
                       :format :raw
                       :handler handle-logout-response}))
                    (dom/props {:class "ra"})
                    (dom/text "Logout"))
                  (LoginPart.)))
            (dom/div (dom/props {:class "fr hhdr"})
              (dom/div (dom/props {:class "fi"})
                (dom/text (e/client current-item-id)))
              (dom/div (dom/props {:class "fi"})
                (case view
                  :main  (do
                          (dom/text "Current tribe: " current-tribe-title))
                  :tribes (dom/text "")))
             ; (when (not= "global" current-tribe-id)
               ; (dom/div (dom/props {:class "fi"})
               ;   (ui/button (e/fn [] ) (dom/text "Join tribe")))
               ; (dom/div (dom/props {:class "fi"})
               ;   (ui/button (e/fn [] ) (dom/text "Join waitlist")))
               ; (dom/div (dom/props {:class "fi"})
                ;  (ui/button (e/fn [] ) (dom/text "Leave tribe"))))
                  );  "Chatroom")))
                ;(dom/text "chatroom-goes-here")
            (case view

              :main (dom/div (dom/props {:class "fr lace"}) (Link-and-Chat-Extended.))
              :tribes (TribesList.))
;            (when (not= view :tribes)
              (dom/div (dom/props {:class "fr"})
                ;(reset! !current-item-xt-id (subs (get-current-path) 8))
                (when current-item-xt-id (ItemView.)))
                ;)

          ;  (dom/h1 (dom/text "welcome to NextApex.co"))
          ;  (dom/p (dom/text "realtime link share"))
          ;  (dom/hr)
          (when (= "R" online-user)
            (dom/div (dom/props {:class "userlistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(user-records db))] (UserItem. id)))))
          ;  (dom/hr)
            (if (not= "" create-account-msg)
              (dom/div (dom/text create-account-msg))
              (if (empty? online-user)
                (CreateAccountPart.)))
            (dom/hr)))))))

(e/defn NewsItem [id rank]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          author (:item/minted-by e)
          item-id (:item/id e)
          item-minted-at (:item/minted-at e)
          link (:item/link e)
          title (:item/title e) ;;hn style is (xor link desc)
          desc (:item/desc e)
          tribe (:item/tribe e)
          upvotes (or (:item/upvotes e) 0)
          time-since-minted (- e/system-time-ms item-minted-at)
          hrs-since-minted (/ time-since-minted 3600)
          gravity 1.1
          score (/ upvotes (Math/pow (+ hrs-since-minted 2) gravity))]
      (e/client
      (let [!vis (atom true)
            vis (e/watch !vis)]
        (dom/div (dom/props {:class ["newsitem" (if (= current-item-id item-id) "selecteditem")]})
          (dom/div (dom/props {:class "fr fg"})
            (dom/div (dom/props {:class "ranking"}) (dom/text rank "."))
            (dom/div (dom/props {:class "fc"})
              (dom/div (dom/props {:class "fr"})
               (when (and vis (not= "" online-user))
                (ui/button (e/fn [] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb
                  [[:xtdb.api/put
                  {:xt/id xt-id
                  :item/link link
                  :item/title title
                  :item/desc desc
                  :item/id item-id
                  :item/tribe tribe
                  :item/minted-by author
                  :item/upvotes (inc upvotes)
                  :item/minted-at item-minted-at}]]))) (e/client (reset! !vis false)))) (dom/props {:class "w"}) (dom/text "+")))
                (dom/div (dom/props {:class "fi fg bb"})
                (dom/a (dom/props {:href link :target "_atarashii"}) (dom/text title))))
              (dom/div (dom/props {:class "fr"})
                (dom/div (dom/props {:class "fi w"})
                (dom/text upvotes " points"))
                (dom/div (dom/props {:class "fi ww"})
                (dom/text "By:" author))
                (dom/div (dom/props {:class "fi ww"})
                (dom/text "⧖" (.toFixed (/ time-since-minted 1000) 2)))
                (dom/div (dom/props {:class "fi ww"})
                (dom/text "↑" (.toFixed score 3)))
                (dom/div (dom/props {:class "fiww"})
                (ui/button (e/fn [] (reset! !current-item-id item-id) (update-url (str (if (= "global" current-tribe-id) "/global/" (str "/tribes/" current-tribe-id "/")) item-id)) (reset! !current-item-xt-id xt-id)) (dom/props {:class "discuss"}) (dom/text "select and discuss")))
                (dom/div (dom/props {:class "fi"})
                (dom/text ""))
                (when (= "R" online-user)
                  (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗"))))))))))))

#?(:cljs (def !ii-link-to-add (atom "")))
(e/def ii-link-to-add (e/client (e/watch !ii-link-to-add)))
#?(:cljs (def !ii-title-to-add (atom "")))
(e/def ii-title-to-add (e/client (e/watch !ii-title-to-add)))
;#?(:cljs (def !ii-desc-to-add (atom "")))
;(e/def ii-desc-to-add (e/client (e/watch !ii-desc-to-add)))
(e/defn NewsItemCreate []
  (e/client
    (dom/div (dom/props {:class "nic"})
       (dom/input (dom/props {:placeholder "Link to add"})
                  (dom/on "change" (e/fn [e]
                                     (reset! !ii-link-to-add (.-value dom/node)))))
       (dom/input (dom/props {:placeholder "Enter a Title"})
                  (dom/on "change" (e/fn [e]
                                     (reset! !ii-title-to-add (.-value dom/node)))))
       ;(dom/input (dom/props {:placeholder "description"})
       ;           (dom/on "change" (e/fn [e]
       ;                              (reset! !ii-desc-to-add (.-value dom/node)))))
       (ui/button
         (e/fn []
            (e/server
              (let [nid (nid)
                    u  (get-in e/*http-request* [:cookies "username" :value])
                    lm (get-in e/*http-request* [:cookies "loginmoment" :value])
                    uh (get-in e/*http-request* [:cookies "userhash" :value])
                    vw (verify-with :argon2 (str lm u) uh)]
                    (when vw
                      (e/discard (e/offload #(xt/submit-tx !xtdb
                        [[:xtdb.api/put
                          {:xt/id nid
                          :item/title ii-title-to-add
                          :item/link (ensure-http-prefix ii-link-to-add)
                          :item/id nid
                          :item/minted-by u
                          :item/upvotes 0
                          :item/tribe current-tribe-id
                          :item/minted-at (System/currentTimeMillis)}]])))))))
                    (dom/text "submit")))))

#?(:clj
   (defn newsitem-records [db tribe-id]
    (let [gravity 1.1]
     (try
       (->> (xt/q db '{:find [(pull ?i [:xt/id :item/minted-by :item/id :item/minted-at :item/link :item/upvotes :item/tribe])]
                        :where [[?i :item/id]
                                [?i :item/tribe tribe]]
                        :in [tribe]} tribe-id)
          (map first)
          (sort-by #(/ (get % :item/upvotes) (Math/pow (+ (/ (- (System/currentTimeMillis) (get % :item/minted-at)) 3600) 2) 1.1)) >) ;;score
          (map-indexed (fn [idx item] (assoc item :rank (inc idx))))
          vec)
        (catch InterruptedException e)))))

#?(:clj
    (defn get-tribe-title-from-id [db tribe-id]
      (->> (xt/q db '{:find [title]
                            :where [[?t :tribe/id tribe]
                                    [?t :tribe/title title]]
                            :in [tribe]} tribe-id)
          first
          first)))

(e/defn ItemView []
  (e/server
    (let [e (xt/entity db current-item-xt-id)
          xt-id (:xt/id e)
          item-xt-id xt-id
          link (:item/link e)
          item-id (:item/id e)
          author (:item/minted-by e)
          title (:item/title e)
          minted-at (:item/minted-at e)
          tribe (:item/tribe e)]
      (e/client
        (when item-id (reset! !current-item-id item-id))
        (dom/div (dom/props {:class "itemview oo"})
          (if (not (= "" current-item-id))
            (do
              (when tribe (reset! !current-tribe-id tribe))
              (dom/div (dom/props {:class "fi"})
                (dom/span (dom/text "Title: ")) (dom/a (dom/props {:href link}) (dom/text title)))
              (dom/div (dom/props {:class "fi"})
                (dom/span (dom/text "url: " link)))
              (dom/div (dom/props {:class "fi"})
                (dom/text "Author: " author))
              (dom/div (dom/props {:class "fi"})
                (dom/text "Tribe: " tribe))
              (dom/div (dom/props {:class "reply-input fi"})
                (ReplyCreate. xt-id xt-id))

              (dom/div (dom/props {:class "replies"})
                (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(reply-with-descendant-records db item-xt-id item-xt-id))]
                  (ItemReply. id)))))))))))

(e/defn ItemReply [xt-id]
  (e/server
    (let [r (xt/entity db xt-id)
          text (:reply/text r)
          author (:reply/minted-by r)
          minted-at (:reply/minted-at r)
          item-xt-id (:reply/item-xt-id r)
          upvotes (:reply/upvotes r)
          parent (:reply/parent-xt-id r)]
      (e/client
        (dom/div (dom/props {:class "itemreplies oo in2"})
          (dom/div (dom/props {:class ""})
            (dom/text text))
          (dom/div (dom/props {:class ""})
            (dom/text author))
          (dom/div (dom/props {:class ""})
            (NestedReplyCreate. current-item-xt-id xt-id))
          (dom/div (dom/props {:class ""})
             (dom/div (dom/props {:class "replies "})
               (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(reply-with-descendant-records db item-xt-id xt-id))] (ItemReplyB. id)))))
          (dom/div (dom/props {:class ""})
           (when (= "R" online-user)
             (ui/button (e/fn [] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn ItemReplyB [xt-id]
  (e/server
    (let [r (xt/entity db xt-id)
          text (:reply/text r)
          author (:reply/minted-by r)
          minted-at (:reply/minted-at r)
          item-xt-id (:reply/item-xt-id r)
          upvotes (:reply/upvotes r)
          parent (:reply/parent-xt-id r)]
      (e/client
        (dom/div (dom/props {:class "itemreplies oo in3"})
          (dom/div (dom/props {:class ""})
            (dom/text text))
          (dom/div (dom/props {:class ""})
            (dom/text author))
          (dom/div (dom/props {:class ""})
            (NestedReplyCreate. current-item-xt-id xt-id))
           (dom/div (dom/props {:class ""})
             (dom/div (dom/props {:class "replies"})
               (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(reply-with-descendant-records db item-xt-id xt-id))] (ItemReplyC. id)))))
          (dom/div (dom/props {:class ""})
           (when (= "R" online-user)
             (ui/button (e/fn [] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn ItemReplyC [xt-id]
  (e/server
    (let [r (xt/entity db xt-id)
          text (:reply/text r)
          author (:reply/minted-by r)
          minted-at (:reply/minted-at r)
          item-xt-id (:reply/item-xt-id r)
          upvotes (:reply/upvotes r)
          parent (:reply/parent-xt-id r)]
      (e/client
        (dom/div (dom/props {:class "itemreplies oo in4"})
          (dom/div (dom/props {:class ""})
            (dom/text text))
          (dom/div (dom/props {:class ""})
            (dom/text author))
          (dom/div (dom/props {:class ""})
            (NestedReplyCreate. current-item-xt-id xt-id))
           (dom/div (dom/props {:class ""})
             (dom/div (dom/props {:class "replies"})
               (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(reply-with-descendant-records db item-xt-id xt-id))] (ItemReplyD. id)))))
          (dom/div (dom/props {:class ""})
           (when (= "R" online-user)
             (ui/button (e/fn [] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn ItemReplyD [xt-id]
  (e/server
    (let [r (xt/entity db xt-id)
          text (:reply/text r)
          author (:reply/minted-by r)
          minted-at (:reply/minted-at r)
          item-xt-id (:reply/item-xt-id r)
          upvotes (:reply/upvotes r)
          parent (:reply/parent-xt-id r)]
      (e/client
        (dom/div (dom/props {:class "itemreplies oo in5"})
          (dom/div (dom/props {:class ""})
            (dom/text text))
          (dom/div (dom/props {:class ""})
            (dom/text author))
          (dom/div (dom/props {:class ""})
           (when (= "R" online-user)
             (ui/button (e/fn [] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗")))))))))

(e/defn ReplyCreate [item-xt-id parent-xt-id]
  (e/client (when (and item-xt-id parent-xt-id (not= "" online-user))
    (InputSubmit. "leave a comment"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb
      [[:xtdb.api/put
        {:xt/id (random-uuid)
        :reply/text v
        :reply/id (nid)
        :reply/minted-by online-user
        :reply/upvotes 0
        :reply/item-xt-id item-xt-id
        :reply/parent-xt-id parent-xt-id ;; i think this should be item-xt-id
        :reply/minted-at (System/currentTimeMillis)}]])))))))))

(e/defn NestedReplyCreate [item-xt-id parent-xt-id]
  (let [masked (atom true)]
    (e/client (when (and item-xt-id parent-xt-id (not= "" online-user))
      (if (e/watch masked)
        (ui/button (e/fn [] (reset! masked false)) (dom/text "reply"))
        (InputSubmit. "nested reply"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb
      [[:xtdb.api/put
        {:xt/id (random-uuid)
        :reply/text v
        :reply/id (nid)
        :reply/minted-by online-user
        :reply/upvotes 0
        :reply/item-xt-id item-xt-id
        :reply/parent-xt-id parent-xt-id
        :reply/minted-at (System/currentTimeMillis)}]])))))))))))

#?(:clj
   (defn reply-with-descendant-records [db item-xt-id parent-xt-id] ;;item is the page we're on, parent is the article or the comment that the comment is referring to
     (try
        (->> (xt/q db '{:find [(pull ?r [:xt/id :reply/text :reply/item-xt-id :reply/parent-xt-id :reply/upvotes])]
                        :where [[?r :reply/item-xt-id item-xt-id]
                                [?r :reply/parent-xt-id parent-xt-id]]
                        :in [item-xt-id parent-xt-id]} item-xt-id parent-xt-id)
          (map first)
          vec
          ;(println)
          )
      (catch InterruptedException e))))

;; tribe list
;; create new tribe
;; visit tribe by clicking = shows [ | | ] []

(e/defn TribeItem [id]
  (e/server
    (let [e (xt/entity db id)
          xt-id   (:xt/id e)
          creator (:tribe/minted-by e)
          tribe-id (:tribe/id e)
          tribe-minted-at (:tribe/minted-at e)
          title (:tribe/title e)
          desc (:tribe/desc e)
          member-count (:tribe/member-count e)]
      (e/client
        (dom/div (dom/props {:class "tribeitem fc"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn []
                                (reset! !current-item-xt-id "")
                                (reset! !current-item-id "")
                                (reset! !current-tribe-title title)
                                (reset! !current-tribe-id tribe-id)
                                (update-url (str "/tribes/" tribe-id "/"))
                                (reset! !view :main))
                        (dom/props {:class "set-tribe"})
                        (dom/text title)))
            (dom/div (dom/props {:class "fi w"})
             (dom/text "0 members"))
          (when (= "R" online-user)
              (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗"))))

          (dom/div (dom/props {:class "fc"})
            (e/server (e/for-by :xt/id [{:keys [xt/id rank]} (e/offload #(newsitem-records db tribe-id))] (NewsItem. id rank)))))))))

(e/defn TribeCreate [] (e/client (dom/div (dom/props {:class "fr"})
  (InputSubmit. "new tribe name"  (e/fn [v] (e/server
    (let [nid (nid)]
      (e/discard (e/offload #(xt/submit-tx !xtdb
        [[:xtdb.api/put
          {:xt/id nid
          :tribe/title v
          :tribe/id nid
          :tribe/minted-by online-user
          :tribe/minted-at (System/currentTimeMillis)}]]))))))))))

#?(:clj
   (defn tribe-records [db]
     (->> (xt/q db '{:find [(pull ?t [:xt/id :tribe/minted-by :tribe/id :tribe/desc :tribe/minted-at :tribe/title :tribe/member-count])]
                     :where [[?t :tribe/id]]})
       (map first)
       (sort-by :tribe/minted-at) ;;or :tribe/member-count
       vec)))

(e/defn TribesList []
  (e/client
    (update-url "/tribes/")
    (dom/div (dom/props {:class "tribelistc fc"})
      (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(tribe-records db))] (TribeItem. id))))
    (dom/br)
    (TribeCreate.)))