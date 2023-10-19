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

#?(:clj
   (def mail-config
     {:host "smtp-relay.sendinblue.com"
      :port 587
      :user "v1nc3ntpull1ng@gmail.com"
      :pass "CcEAZqTdVrDfRPzS"
      :tls true}))

#?(:cljs (defn set-scroll-position [x y] (.scrollTo js/window x y)))
#?(:cljs (defn open-url [url] (.open js/window url)))
#?(:cljs (defn get-current-path [] (.-pathname js/location)))
#?(:cljs (defn replace-state [state title url] (.replaceState js/history state title url)))
#?(:cljs (defn update-url [url] (replace-state nil "" url)))

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
        (when (not= "" online-user) (NewsItemCreate.))))
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
                                 (set! (.-value dom/node) ""))))))))))

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
#?(:clj (defn ensure-http-prefix [url] (if (.startsWith url "http://") url (str "http://" url))))

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
      (ui/button (e/fn [] (reset! !create-account-visible true)) (dom/text "create an account...")))))

(e/defn Todo-list []
  (e/server (let [username-from-http-req (e/server (get-in e/*http-request* [:cookies "username" :value]))]
    (binding [!xtdb user/!xtdb
              db (new (db/latest-db> user/!xtdb))]
        (e/client
          (if (some? username-from-http-req) (reset! !online-user username-from-http-req)) ;;check session, set cljs var
          (dom/div (dom/props {:class "bigc"})
            (dom/div (dom/props {:class "fr hdr"})
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
                  (LoginPart.)))
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                (dom/text (e/client current-item-id)))
              (dom/div (dom/props {:class "fi"})
                (case view
                  :main  (do
                          (dom/text "Current Tribe: " current-tribe-view))
                  :tribes (dom/text "Ocean")))
              (dom/div (dom/props {:class "fi"})
                (dom/text current-tribe-view " chatroom")))
            (dom/div (dom/props {:class "fr"})
              (dom/div (dom/props {:class "fi"})
                ;(dom/text "chatroom-goes-here")
                (case view
                  
                  :main (Link-and-Chat-Extended. "placeholder-username"))))
            (dom/div (dom/props {:class "fr"})
              (reset! !current-item-xt-id (subs (get-current-path) 1))
              (when current-item-xt-id (ItemView.)))
            (dom/h1 (dom/text "welcome to NextApex.co"))
            (dom/p (dom/text "realtime link share"))
            (dom/hr)
            (dom/div (dom/props {:class "userlistc fc"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(user-records db))] (UserItem. id))))
            (dom/hr)
            (if (not= "" create-account-msg) 
              (dom/div (dom/text create-account-msg))
              (if (empty? online-user)
                (CreateAccountPart.)))
            (dom/hr)))))))

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
          upvotes (or (:item/upvotes e) 0)
          time-since-minted (- e/system-time-ms item-minted-at)
          hrs-since-minted (/ time-since-minted 3600)
          gravity 1.5
          score (/ upvotes (Math/pow (+ hrs-since-minted 2) gravity))]
      (e/client
        (dom/div (dom/props {:class "newsitem fing"})
          (dom/div (dom/props {:class "fr"})
            (dom/div (dom/props {:class "fi"})
             (dom/text upvotes))
            (ui/button (e/fn [] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
              [[:xtdb.api/put
              {:xt/id xt-id
              :item/link link
              :item/title title
              :item/desc desc
              :item/id item-id
              :item/minted-by author
              :item/upvotes (inc upvotes)
              :item/minted-at item-minted-at}]]))))) (dom/text "+"))
            (dom/div (dom/props {:class "fi fg bb"})
             (dom/text link))
            (dom/div (dom/props {:class "fi"})
             (dom/text "By:" author))
            (dom/div (dom/props {:class "fi"})
             (dom/text "⧖ " time-since-minted))
            (dom/div (dom/props {:class "fi"})
             (dom/text "↑ " (.toFixed score 3)))
            (dom/div (dom/props {:class "fi"})
             (ui/button (e/fn [] (reset! !current-item-id item-id) (update-url item-id) (reset! !current-item-xt-id xt-id)) (dom/text "discuss")))
            (dom/div (dom/props {:class "fi"})
             (when (= "R" online-user) 
              (ui/button (e/fn [v] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗"))))))))))

(e/defn NewsItemCreate [] (e/client (InputSubmit. "link to add"  (e/fn [v] (e/server 
  (let [nid (nid)]
    (e/discard (e/offload #(xt/submit-tx !xtdb 
    [[:xtdb.api/put
      {:xt/id nid
      :item/link (ensure-http-prefix v)
      :item/id nid
      :item/minted-by online-user
      :item/upvotes 0
      :item/minted-at (System/currentTimeMillis)}]])))))))))

#?(:clj
   (defn newsitem-records [db]
     (->> (xt/q db '{:find [(pull ?i [:xt/id :item/minted-by :item/id :item/minted-at :item/link :item/upvotes])]
                     :where [[?i :item/id]]})
       (map first)
       (sort-by #(/ (get % :item/upvotes) (Math/pow (+ (/ (- (System/currentTimeMillis) (get % :item/minted-at)) 3600) 2) 1.5)) >) ;;score
       vec)))

(e/defn ItemView []
  (e/server
    (let [e (xt/entity db current-item-xt-id)
          xt-id (:xt/id e)
          item-xt-id xt-id
          link (:item/link e)
          item-id (:item/id e)
          author (:item/minted-by e)
          minted-at (:item/minted-at e)]
      (e/client
        (dom/div (dom/props {:class "itemview oo"})
          (dom/div (dom/props {:class "fi"})
            (dom/span (dom/text "Current link: ")) (dom/a (dom/props {:href link}) (dom/text link)))
          (dom/div (dom/props {:class "fi"})
            (dom/text "Author: " author))
          (dom/div (dom/props {:class "reply-input fi"})
            (ReplyCreate. xt-id xt-id))
        (dom/div (dom/props {:class "replies"})
          (e/server (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(reply-with-descendant-records db item-xt-id item-xt-id))] (ItemNestedReplies. id)))))))))

(e/defn ItemNestedReplies [xt-id]
  (e/server
    (let [r (xt/entity db xt-id)
          text (:reply/text r)
          author (:reply/minted-by r)
          minted-at (:reply/minted-at r)
          item-xt-id (:reply/item-xt-id r)
          upvotes (:reply/upvotes r)
          parent (:reply/parent-xt-id r)
          replies (e/offload #(reply-with-descendant-records db item-xt-id xt-id))]
        (e/client
          (dom/div (dom/props {:class "itemreplies oo in1"})
            (dom/div (dom/props {:class ""})
              (dom/text text))
            (dom/div (dom/props {:class ""})
              (dom/text author))
            (dom/div (dom/props {:class ""})
              (NestedReplyCreate. current-item-xt-id xt-id))        
            (dom/div (dom/props {:class "replies"})
              (e/server (e/for-by :xt/id [{:keys [xt/id]} replies] (ItemReply. id)))))
            (dom/div (dom/props {:class ""})
             (when (= "R" online-user) 
              (ui/button (e/fn [] (e/server (e/discard (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]])))) (dom/text "✗"))))))))

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
  (e/client (when (and item-xt-id parent-xt-id) 
    (InputSubmit. "leave a comment"  (e/fn [v] (e/server (e/discard (e/offload #(xt/submit-tx !xtdb 
      [[:xtdb.api/put
        {:xt/id (random-uuid)
        :reply/text v
        :reply/id (nid)
        :reply/minted-by online-user
        :reply/upvotes 0
        :reply/item-xt-id item-xt-id
        :reply/parent-xt-id parent-xt-id
        :reply/minted-at (System/currentTimeMillis)}]])))))))))

(e/defn NestedReplyCreate [item-xt-id parent-xt-id] 
  (let [masked (atom true)]
    (e/client (when (and item-xt-id parent-xt-id) 
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
   (defn reply-with-descendant-records [db item-xt-id parent-xt-id]
     (->> (xt/q db '{:find [(pull ?r [:xt/id :reply/text :reply/item-xt-id :reply/parent-xt-id :reply/upvotes])]
                     :where [[?r :reply/item-xt-id item-xt-id]
                             [?r :reply/parent-xt-id parent-xt-id]]
                     :in [item-xt-id parent-xt-id]} item-xt-id parent-xt-id)
       (map first)
       vec)))
