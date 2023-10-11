(ns app.todo-list
  (:require #?(:clj [app.xtdb-contrib :as db])
            [cljs.js :as js]
            [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:clj [cryptohash-clj.api :as ch :refer [hash-with verify-with]])
            #?(:cljs [goog.events :as events])
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

(defn ni
  "Mints a new album id."
  []
;;todo
;;check for collisions before minting a new one.
;;must not collide on:
;; +album-id
  (subs (str (random-uuid)) 0 6))

(defn nit
  "Mints a new track id."
  []
  (subs (str (random-uuid)) 9 18))

(defn nir
  "Mints a new track id, rating id."
  []
  (subs (str (random-uuid)) 6 16))

(e/def !xtdb)
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (e/watch !msgs))))

#?(:clj (defonce !present (atom {}))) ; session-id -> user
(e/def present (e/server (e/watch !present)))

#?(:clj (defonce !eventses (atom []))) ; eventses are coll of event-type & timestamp and only accepts timestamps within :big-window: while interface displays :lil-window: worth of notifs
(e/def eventses (e/server (e/watch !eventses)))

#?(:clj
   (defn add-event-notif [event-type t]
    (swap! !eventses (fn [es]
                      (-> (if (>= (count es) 300)  ;;max out at 300 because why not
                            (drop-last es) ; drop the oldest notification
                            es)
                          (conj {:event-type event-type :timestamp t}))))))

;on an event, add to the map
; in the interface, use a when to render the item
;; use them css them to float the note upwards.
;;; choose notes and items for the om1 and om3 sets

#?(:cljs (defn get-current-path []
           (.-pathname js/location)))

#?(:cljs (defn replace-state [state title url]
           (.replaceState js/history state title url)))

#?(:cljs (defn update-url [url]
           ;(reset! history (get-current-path))
           ;(replace-state nil "" url)
           ))

#?(:cljs (defonce stripe-instance (atom nil)))
#?(:cljs (defonce stripe-elements (atom nil)))
#?(:cljs (defonce payment-element (atom nil)))
#?(:cljs (defonce card-element-atom (atom nil)))

#?(:cljs
   (defn mount-stripe-card []
     (let [S @stripe-instance
           elements (.elements S)
           cardElement (.create elements "card")
           cardNode (js/document.getElementById "card-element")]
           (.log js/console S cardNode "$!&")
       (.mount cardElement cardNode)
       (reset! card-element-atom cardElement))))

#?(:cljs
   (defn init-stripe []
     (reset! stripe-instance (js/Stripe. "pk_live_51NDMUNCiKuA9GKLbSloPeOpiPKS6LjKUFsvzIWvu76ywfOz9dlNV1xWeaNGKiw8sVse5XJZ8u5N7eqmbsHCBQl5800XACX4fsL"))
     (.log js/console "attemts mounte stripe carde")
     (mount-stripe-card)
     (.log js/console "stripe initalized")))

(e/defn PaymentForm []
  (e/client
   (dom/form (dom/props {:id "payment-form"})
             (dom/div (dom/props {:id "card-element"}) (dom/text "Waiting on availability of https to generate Card input"))
             (dom/div (dom/props {:id "card-errors" :role "alert"})))))

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
       (dom/text "Get Music Lover Membership to login to chat."))
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

#?(:clj
   (defn the-mine-user-record [db current-remewser]
     (try
       (->> (xt/q db '{:find [(pull ?e [:xt/id :user/username :user/id :user/review-ids :user/rating-ids :user/remewserid :user/ratings :user/email :user/img :user/created-at :user/created-by :user/modified-at :user/password])]
                     :where [[?e :user/remewserid crid]]
                     :in [crid]}
                current-remewser) ;"RMWS|WSR|V000|7"
          (map first)
          vec)
      (catch InterruptedException e))))

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
   (when (not (empty? (get-in e/*http-request* [:cookies "remewserid" :value])))
     (e/client
      (dom/input (dom/props {:placeholder (or ph "")})
                 (dom/on "keydown" (e/fn [e]
                                     (when (= "Enter" (.-key e))
                                       (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                         (new F v)
                                         (set! (.-value dom/node) ""))))))))))

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

#?(:cljs (def viewmap
           {"/" :main-page
            "/user-list" :user-list
            "/track-list" :track-list}))

#?(:cljs (def !view (atom :main-page))) 
(e/def view (e/watch !view))
;#?(:cljs (defonce !current-album-id (atom "859d33")))
;(e/def current-album-id (e/watch !current-album-id))
#?(:cljs (defonce !current-track-id (atom "")))
(e/def current-track-id (e/watch !current-track-id))
#?(:cljs (defonce !xt-id (atom "")))
(e/def xt-id (e/watch !xt-id))
#?(:cljs (defonce !current-project-name (atom "")))
(e/def current-project-name (e/watch !current-project-name))

#?(:cljs (defonce !current-convo-id (atom "")))
(e/def current-convo-id (e/watch !current-convo-id))
#?(:cljs (defonce !current-convo-title (atom "")))
(e/def current-convo-title (e/watch !current-convo-title))


(e/defn ui-nav-bttns []
  (e/server
   (when (not (empty? (get-in e/*http-request* [:cookies "remewserid" :value])))
     (e/client
      (dom/div (dom/props {:class "uinva"})
               (ui/button
                (e/fn [] (e/client
                          (do (update-url "/track-list")
                              (reset! !view :track-list))))
                (dom/text "Track List"))
               (ui/button
                (e/fn [] (e/client
                          (do (update-url "/user-list")
                              (reset! !view :user-list))))
                (dom/text "User List"))
               (ui/button
                (e/fn [] (e/client
                          (do (update-url "/preview")
                              (reset! !view :preview-page))))
                (dom/text "Preview Page"))
               (ui/button
                (e/fn [] (e/client
                          (do (update-url "/submit")
                              (reset! !view :submit-page))))
                (dom/text "Submit Page"))
               (ui/button
                (e/fn [] (e/client
                          (do (update-url "/settings")
                              (reset! !view :settings-page))))
                (dom/text "Settings Page"))
              
               (ui/button
                (e/fn [] (e/client
                          (do 
                              (reset! !view :onvo-list))))
                (dom/text "Onvo Page"))

               (ui/button
                (e/fn [] (e/client
                          (do 
                              (reset! !view :convo-list))))
                (dom/text "Convo Page"))

               (ui/button
                (e/fn [] (e/client
                          (do 
                              (reset! !view :convo-list-buttons))))
                (dom/text (str "Convo Buttons | " current-convo-title)))

               (ui/button (e/fn [] (e/client (do (update-url "/") (reset! !view :main-page))))
                          (dom/text "Switch to Main Page"))

               (ui/button (e/fn [] (e/client
                                    (do (update-url (str "/create-account"))
                                        (reset! !view :create-account-page))))
                          (dom/text "Create Account Page"))
               (ui/button (e/fn [] (e/client
                                    (do (update-url (str "/inspek"))
                                        (reset! !view :inspect-db))))
                          (dom/text "BES Page")))))))

;; rating-ids are the ratings you made
;; ratings are the ratings given to you by others

#?(:clj
   (defn user-records [db]
    (try
     (->> (xt/q db '{:find [(pull ?e [:xt/id :user/email :user/remewserid :user/created-at])]
                     :where [[?e :user/email]]})
          (map first)
          (sort-by :user/created-at)
          vec)
    (catch InterruptedException e))))

(e/defn MainPage []
 (e/server
  (let [cust-count (count (e/offload #(user-records db)))
        ;wass-cust-count (count recs)
        ]
   (e/client
    (let [cmpi (* 0.33 cust-count)
          cmmp (.toFixed (+ 11.11 (* 0.33 cust-count)) 2)]
      (dom/div (dom/props {:class "rmws-mn"})
               (dom/h3 (dom/props {:class "lyne"}) (dom/text "Remuse: Listening Party Central"))

               (LatestAdditionsOne.)


               (LatestAdditionsList. 4))
               (Notifications.)
      (dom/div (dom/props {:class "rhb"}) (dom/text "Remuse"))
      (dom/div (dom/props {:class "wii2"})
               (dom/h3 (dom/text "Welcome to Remuse!"))
               (dom/p (dom/text "We're a vibrant community of music lovers and musicians where you get to have a direct impact on what music gets shared and celebrated. Want to join in the fun? Let's talk about our ")
                      (dom/strong (dom/text "unique and fair pricing model.")))
               (dom/p
                (dom/span (dom/text "We believe in "))
                (dom/em (dom/text "rewarding early birds "))
                (dom/span (dom/text "and encouraging a sustainable platform growth, so "))
                (dom/strong (dom/text "our membership price increases slightly with each new sign-up. "))
                (dom/span (dom/text "Sounds intriguing, right? Let us break it down:"))
                (dom/br)
                (dom/ol (dom/li (dom/strong (dom/text "Musician / Music Lover "))
                                (dom/span (dom/text (str "The journey begins at $11.11/year, and with each new listener, the price increases by $0.33.  The current price is at $" cmmp "/year.
                       With Music Lover membership, you get to vote on tracks and reviews, engage in lively listening party chats, and the ability to add songs to our review queue.")))))
                (dom/br)
                (dom/div (dom/props {:class "rmsze"})
                         (dom/span (dom/text "* Price increases by 33 cents every time a new person signs up.  You can learn more about our pricing model "))
                         (dom/a (dom/props {:href "https://remuser001.wordpress.com/2023/07/11/our-unique-pricing-model-at-remuse/"}) (dom/text "here")))
                (dom/p (dom/text "With Remuse, every penny you spend goes directly into creating a richer and more rewarding experience for you and your fellow music enthusiasts.  Musicians benefit by getting exposure, getting seen and heard faster, and listeners benefit by discovering fresh new music, often.  And remember, the earlier you join, the less you pay."))
                (dom/p (dom/text "Let the music guide you, and   ") (dom/props {:class "fl"})
                       (dom/div (dom/props {:class "subz"}) (ui/button (e/fn [] (e/client (reset! !view :create-account-page) (set-scroll-position 0 0) (update-url "create-account"))) (dom/text "let's turn up the volume together!") (dom/props {:class "subhuti"})))))))))))
  ;; mesh score

(e/defn Notifications []
 (e/client
 ; (dom/div (dom/props {:class "flex-item"}) (dom/text eventses))
  (e/for [{:keys [timestamp event-type]} eventses]
    (dom/div 
    (dom/props {:class "notif" }) 
    (dom/text 
      (case event-type
            :new-global-chat-msg "♪"
            :new-track-desc "𝄢"
            :ca "𝄫"
            :ef "𝄑"
            :ga "♩"
            :hi "♪"
            :na "♮"
            :sha "♯"
            :new-track-desc-review "❏"
            :new-track-review "❍"
            :default event-type))))))

(e/defn TrackByIdPage [track-xt-id chat-enabled?]
 (e/server
  (let [cust-count (count (e/offload #(user-records db)))]
   (e/client
    (let [cmpi (* 0.33 cust-count)
          cmmp (.toFixed (+ 11.11 (* 0.33 cust-count)) 2)]
      (dom/div (dom/props {:class "rmws-mn"})
               (dom/h3 (dom/props {:class "lyne"}) (dom/text "Remuse: Listening Party Central"))
               (LatestAdditionTrackItemWithChat. track-xt-id chat-enabled?)))))))

(e/defn ProjectByNamePage [project-str] 
  (e/client
    (let [z 0]
      (dom/div (dom/props {:class "tracks-fo-project-by-name"})
        (e/server
          (e/for-by :xt/id [{:keys [xt/id]}  (e/offload #(tracks-fo-project db project-str))]
                (LatestAdditionTrackItemWithChat. id false)))))))

(e/defn LatestAdditionsPage []
  (e/client
   (dom/div (dom/props {:class "rmws-mn"})
            (dom/h3 (dom/text "LATEST REMUSE ADDITIONS"))
            (dom/div (dom/props {:class "emc"})
                     (dom/text "Remuse is a music sharing place, built for musicians, by musicians, to enable musicians to discover new sounds, and to have their sounds heard.  Get exposure and get new inspiration.  As a musician you can add tracks to our database, guaranteeing an editor review, and getting your band or project the attention it deserves.  Get a pro account to write reviews and skip the queue. "))
            (dom/div (dom/props {:class "emc"}))
            (dom/div (dom/props {:class "emp" :data "editor mini preview"}) (dom/text (str (* 100 (rand)))))
            (dom/text "Latest Additions")
            (LatestAdditionsList. 7))))

(e/defn LatestAdditionsList "drops one to take t" [t]
  (e/client
   (dom/div
    (dom/props {:class "latest-addition-list"})
    (dom/div
     (dom/props {:class "latest-addition-items"})
     (e/server
      (e/for-by :xt/id [{:keys [xt/id]} (drop 1 (take t (e/offload #(latest-addition-track-records db))))]
                (LatestAdditionTrackItemWithChat. id false)))))))

(e/defn LatestAdditionsOne []
  (e/client
   (dom/div
    (dom/props {:class "latest-addition-list"})
    (dom/div
     (dom/props {:class "latest-addition-items"})
     (e/server
      (e/for-by :xt/id [{:keys [xt/id]} (take 1 (e/offload #(latest-addition-track-records db)))]
                (LatestAdditionTrackItemWithChat. id true)))))))

(e/defn LatestAdditionTrackItemWithChat [id chat-on-yes-or-no]
  (e/server
   (let [e (xt/entity db id)
         title        (:track/title e)
         track-id     (:track/id e)
         track-art    (:track/art e)

         created-at   (:track/created-at e)
         artist       (:track/artist e)
         album-id     (:track/album-id e)
         track-number (:track/number e)
         ;associated-project (:track/project-id e)
         spotify-link (:track/spotify-link e)
         youtube-link (:track/youtube-link e)
         applemusic-link (:track/applemusic-link e)
         project artist
         ;album-rez (-> (xt/q db '{:find [?title xt-id]
         ;                         :where [[?a :xt/id xt-id]
         ;                                 [?a :album/id album-id]
         ;                                 [?a :album/title ?title]]
         ;                         :in [album-id]}
         ;                    album-id)
         ;              first)
         ;album-uuid (-> album-rez second)
         ;album-title (-> album-rez first)
         ;project-name (-> (xt/q db '{:find [?title]
         ;                            :where [[?p :project/id associated-project]
         ;                                    [?p :project/title ?title]]
         ;                            :in [associated-project]}
         ;                       associated-project)
         ;                 first first)
         ;album-e (xt/entity db album-uuid)
         ;album-art (:album/art album-e)
         ratings (-> (xt/q db '{:find [?xt-id ?rating-target ?rating-value ?created-by ?rating-value]
                                :where [[?r :xt/id ?xt-id]
                                        [?r :rating/value ?rating-value]
                                        [?r :rating/target ?rating-target]
                                        [?r :rating/created-by ?created-by]]
                                :in [?rating-target]}
                           track-id)) ;;id is the track-id is the rating-target


         ratings-by-this-user (-> (xt/q db '{:find [[?rating-xt-id ?rating-id ?rating-target ?rating-value ?rating-value]]
                                             :where [[?r :xt/id ?rating-xt-id]
                                                     [?r :rating/id ?rating-id]
                                                     [?r :rating/value ?rating-value]
                                                     [?r :rating/target ?rating-target]
                                                     [?r :rating/created-by ?created-by]]
                                             :in [?rating-target ?created-by]}
                                        track-id (get-in e/*http-request* [:cookies "remewserid" :value])) ;;track-id is the rating-target, created-by is this get-in (2 arguments on this line)
                                  first first)
         rating-xt-id (first ratings-by-this-user)
         rating-id (second ratings-by-this-user)
         rrz (into [] ratings)
         rating-count (count rrz)
         rating-score (-> (reduce +
                                  (map (fn [sub-vec]
                                         (cond (= (nth sub-vec 2) :doubleplus) 99
                                               (= (nth sub-vec 2) :plus) 71
                                               (= (nth sub-vec 2) :minus) 31
                                               :else 0)) rrz))
                          (/ (if (= 0 rating-count) 1 rating-count))
                          (int))
         ;add review for each one, if it exists already.
         track-reviews (->> (xt/q db '{:find [(pull ?review [:xt/id :review/id :review/track-id :review/track-review :review/created-at :review/created-by])]
                                       :where [[?review :review/track-id track-id]]
                                       :in [track-id]}
                                  track-id)
                            (map first)
                            vec)]
     (e/client
      (dom/div (dom/props {:class "qirline" :id id}) ;;put the xt-id in the HTML so can rest peacefully
               (dom/img (dom/props {:class "minipic" :src (str "img/" track-art)}))
               (dom/div (dom/props {:class "qidsquarecontain"})
                        ;(when chat-on-yes-or-no (dom/div (dom/props {:class "culp"}) (dom/text "Current Listening Party")))
               ;(dom/div (dom/text (count ratings)))
               ;; needs be ui/button w/ set album on cleeck
               ;;(SpotButan. spotify-link project-name title)
                        (dom/div (dom/props {:class "hidarifo"})
                                 (when chat-on-yes-or-no (dom/div (dom/props {:class "chat-ext"}) (ChatExtended.))))
                        (dom/div (dom/props {:class "aifo"})
                                 
                                 ;;was:(dom/div (dom/props {:class "qidbloc"}) (dom/text (str title)))
                                 ;;now:
                                 (ui/button (e/fn [] (e/client (reset! !view :track-by-id) (reset! !xt-id id) (set-scroll-position 0 0))) (dom/props {:class "qidbloc"}) (dom/text title))

                                 ;(when (= "v1nc3ntpull1ng@gmail.com" (e/server (get-in e/*http-request* [:cookies "useremail" :value])))
                                 ;  (ui/button (e/fn [] (e/client (reset! !view :track-by-id) (reset! !xt-id id) (set-scroll-position 0 0))) (dom/props {:class "xt-track-btn"}) (dom/text id)))
                                ; (dom/div (dom/props {:class "qidbloc"}) (dom/text (str project)))
                                 (ui/button (e/fn [] (e/client (reset! !view :project-by-name) (reset! !current-project-name project) (set-scroll-position 0 0))) (dom/props {:class "qidbloc"}) (dom/text project))
                                 (when spotify-link (dom/div (dom/props {:class "splnk"}) (dom/a (dom/props {:href (str "https://open.spotify.com/track/" spotify-link)}) (dom/text "[Spotify]"))))
                                 (when youtube-link (dom/div (dom/props {:class "ytlnk"}) (dom/a (dom/props {:href (str "https://www.youtube.com/watch?v=" youtube-link)}) (dom/text "[Youtube]"))))
                                 (when applemusic-link (dom/div (dom/props {:class "ytlnk"}) (dom/a (dom/props {:href (str "https://music.apple.com/" applemusic-link)}) (dom/text "[Apple Music]"))))
                                 

                                                       (dom/div (dom/props {:class "review-cool"})
                                                                (e/server
                                                                 (e/for-by :review/review-id [{:keys [xt/id review/review-id review/track-id review/track-review review/created-at review/created-by review/modified-at]} track-reviews]
                                                                           (let [ratings (-> (xt/q db '{:find [?xt-id ?rating-target ?rating-value ?created-by ?rating-value]
                                                                                                        :where [[?r :xt/id ?xt-id]
                                                                                                                [?r :rating/value ?rating-value]
                                                                                                                [?r :rating/target ?rating-target]
                                                                                                                [?r :rating/created-by ?created-by]]
                                                                                                        :in [?rating-target]}
                                                                                                   id)) ;;id is the review-id (is the uuid which is :xt/id) is the rating-target


                                                                                 ratings-by-this-user (-> (xt/q db '{:find [[?rating-xt-id ?rating-id ?rating-target ?rating-value ?rating-value]]
                                                                                                                     :where [[?r :xt/id ?rating-xt-id]
                                                                                                                             [?r :rating/id ?rating-id]
                                                                                                                             [?r :rating/value ?rating-value]
                                                                                                                             [?r :rating/target ?rating-target]
                                                                                                                             [?r :rating/created-by ?created-by]]
                                                                                                                     :in [?rating-target ?created-by]}
                                                                                                                id (get-in e/*http-request* [:cookies "remewserid" :value])) ;;id is the review-id is the rating-target, created-by is this get-in (2 arguments on this line) ;;great note
                                                                                                          first first)
                                                                                 rating-xt-id (first ratings-by-this-user)
                                                                                 rating-id (second ratings-by-this-user)
                                                                                 rrz (into [] ratings)
                                                                                 rating-count (count rrz)
                                                                                 review-rating-score (-> (reduce +
                                                                                                                 (map (fn [sub-vec]
                                                                                                                        (cond (= (nth sub-vec 2) :doubleplus) 99
                                                                                                                              (= (nth sub-vec 2) :plus) 71
                                                                                                                              (= (nth sub-vec 2) :minus) 31
                                                                                                                              :else 0)) rrz))
                                                                                                         (/ (if (= 0 rating-count) 1 rating-count))
                                                                                                         (int))]
                                                                             (e/client
                                                                              (dom/div (dom/props {:class "review-cool-elem"})
                                                                                       (dom/div (dom/props {:class "rmwsrvid"}) (dom/text (e/server (when (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value]))) id))))
                              ;(dom/div (dom/props {:class "track-id"}) (dom/text track-id))
                              ;(dom/div (dom/props {:class "created-at"}) (dom/text created-at))
                              ;(dom/div (dom/props {:class "created-by"}) (dom/text created-by))
                                                                                       (dom/div (dom/props {:class "track-review"})
                                                                                                (dom/div (dom/props {:class "track-review-content"}) (dom/text track-review))
                                                                                                (dom/div (dom/props {:class "track-review-rating"})
                                                                                                         (dom/div (dom/props {:class "track-rating-butans"})
                                                                                                                  (dom/div (dom/props {:class "rrcc"})
                                                                                                                           (dom/div (dom/props {:class "rrccrrcc"})
                                                                                                                                    (when (not (= 0 review-rating-score)) (dom/text review-rating-score)))
                                                                                                                           (dom/div (dom/props {:class "ratecontain"})
                                                                                                                                    (ui/button
                                                                                                                                     (e/fn []
                                                                                                                                       (e/server
                                                                                                                                        (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                                                                                                          (e/discard
                                                                                                                                           (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                                                                                                                 {:xt/id (or rating-xt-id (random-uuid))
                                                                                                                                                                  :rating/id (or rating-id (nir))
                                                                                                                                                                  :rating/created-at (or created-at e/system-time-ms)
                                                                                                                                                                  :rating/modified-at e/system-time-ms
                                                                                                                                                                  :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                                                                                                                  :rating/target id
                                                                                                                                                                  :rating/value :doubleplus}]]))
                                                                                                                                          (e/client (reset! client-message "Must have musician membership to select ratings for reviews.")))))
                                                                                                                                     (dom/props {:class "dblp bbmini"})
                                                                                                                                     (dom/text "++")))
                                                                                                                           (dom/div (dom/props {:class "ratecontain"})
                                                                                                                                    (ui/button
                                                                                                                                     (e/fn []
                                                                                                                                       (e/server
                                                                                                                                        (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                                                                                                          (e/discard
                                                                                                                                           (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                                                                                                                 {:xt/id (or rating-xt-id (random-uuid))
                                                                                                                                                                  :rating/id (or rating-id (nir))
                                                                                                                                                                  :rating/created-at (or created-at e/system-time-ms)
                                                                                                                                                                  :rating/modified-at e/system-time-ms
                                                                                                                                                                  :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                                                                                                                  :rating/target id
                                                                                                                                                                  :rating/value :plus}]]))
                                                                                                                                          (e/client (reset! client-message "Must have musician membership to select ratings for reviews.")))))
                                                                                                                                     (dom/props {:class "sngp bbmini"})
                                                                                                                                     (dom/text "+")))
                                                                                                                           (dom/div (dom/props {:class "ratecontain"})
                                                                                                                                    (ui/button
                                                                                                                                     (e/fn []
                                                                                                                                       (e/server
                                                                                                                                        (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                                                                                                          (e/discard
                                                                                                                                           (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                                                                                                                 {:xt/id (or rating-xt-id (random-uuid))
                                                                                                                                                                  :rating/id (or rating-id (nir))
                                                                                                                                                                  :rating/created-at (or created-at e/system-time-ms)
                                                                                                                                                                  :rating/modified-at e/system-time-ms
                                                                                                                                                                  :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                                                                                                                  :rating/target id
                                                                                                                                                                  :rating/value :minus}]]))
                                                                                                                                          (e/client (reset! client-message "Must have musician membership to select ratings for reviews.")))))
                                                                                                                                     (dom/props {:class "sngn bbmini"})
                                                                                                                                     (dom/text "-"))))))))))))
                                                                (dom/span (dom/text (str "")) ;id " | " title " | " rating-id " | " rating-xt-id " | " rating-score))
                                                                          (dom/span (dom/props {:class "rax"}) (dom/text (str ""))))))

                              ;(dom/div (dom/props {:class "ca"}) (dom/text (str "at " created-at)))


                        (dom/div (dom/props {:class ["rc" "rcb"]})
                                 (dom/div (dom/props {:class "rcrc"})
                                          (when (not (= 0 rating-score)) (dom/text rating-score)))
                                 (dom/div (dom/props {:class "ratecontain"})
                                          (ui/button
                                           (e/fn []
                                             (e/server
                                              (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                (e/discard
                                                 (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                       {:xt/id (or rating-xt-id (random-uuid))
                                                                        :rating/id (or rating-id (nir))
                                                                        :rating/created-at e/system-time-ms
                                                                        :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                        :rating/target track-id
                                                                        :rating/value :doubleplus}]]))
                                                (e/client (reset! client-message "Must have musician membership to select ratings for tracks.")))))
                                           (dom/props {:class "dblp b"})
                                           (dom/text "++")))
                                 (dom/div (dom/props {:class "ratecontain"})
                                          (ui/button
                                           (e/fn []
                                             (e/server
                                              (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                (e/discard
                                                 (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                       {:xt/id (or rating-xt-id (random-uuid))
                                                                        :rating/id (or rating-id (nir))
                                                                        :rating/created-at e/system-time-ms
                                                                        :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                        :rating/target track-id
                                                                        :rating/value :plus}]]))
                                                (e/client (reset! client-message "Must have musician membership to select ratings for tracks.")))))
                                           (dom/props {:class "sngp b"})
                                           (dom/text "+")))
                                 (dom/div (dom/props {:class "ratecontain"})
                                          (ui/button
                                           (e/fn []
                                             (e/server
                                              (if (not (nil? (get-in e/*http-request* [:cookies "remewserid" :value])))
                                                (e/discard
                                                 (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                       {:xt/id (or rating-xt-id (random-uuid))
                                                                        :rating/id (or rating-id (nir))
                                                                        :rating/created-at e/system-time-ms
                                                                        :rating/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                        :rating/target track-id
                                                                        :rating/value :minus}]]))
                                                (e/client (reset! client-message "Must have musician membership to select ratings for tracks.")))))
                                           (dom/props {:class "sngn b"})
                                           (dom/text "-"))))
               ;;was .trv
                        (dom/div (dom/props {:class "tri"})
                                 (InputSubmit. "track review / music words"
                                               (e/fn [v]
                                                 (e/server
                                                  (e/discard
                                                   (xt/submit-tx !xtdb [[:xtdb.api/put
                                                                         {:xt/id (random-uuid)

                                                                          :review/review-id (ni)
                                                                          :review/created-at e/system-time-ms
                                                                          :review/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                                          :review/track-id track-id
                                                                          :review/track-review v}]]))
                                                  (add-event-notif :new-track-desc (. System (currentTimeMillis)))))))))))))

#?(:cljs (def !e-mail-for-login (atom " ")))
#?(:cljs (def !sl (atom false)))
(e/def sloaded (e/watch !sl))
#?(:cljs (e/def e-mail-for-login (e/watch !e-mail-for-login)))
#?(:cljs (defn valid-email? [email]
           (re-matches #"(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$" email)))
#?(:cljs (def !sck (atom "")))
(e/def sck (e/watch !sck))

#?(:clj
   (defn extract-client-secret [response]
     (second (re-find #"\"client_secret\":\s*\"([^\"]+)\"" response))))

#?(:clj 
   (defn sanitize [bcrypt-hash]
    (java.net.URLEncoder/encode bcrypt-hash "UTF-8")))

#?(:clj 
  (defn mint-new-membership [e]
    ;(println "Mintang a new mambarsheeep : " e)
    (let [email e
          user-count (->> (xt/q (xt/db user/!xtdb) '{:find [(pull ?e [:xt/id :user/username :user/id :user/review-ids :user/rating-ids :user/remewserid :user/ratings :user/email :user/img :user/created-at :user/created-by :user/modified-at :user/password])]
                                     :where [[?e :user/id]]})
                          (map first)
                          (sort-by :user/id)
                          vec
                          count
                          inc
                          inc) ;we are doing user-count +2
          in-ts (.getTime (java.util.Date.)) ;;get-current-time
          raw (str user-count in-ts)
          hash (hash-with :bcrypt raw)
          sanitized (sanitize hash) 
          domain "https://remuse.co"
          link (str domain "/authentim8?uc=" user-count "&timestamp=" in-ts "&code=" sanitized)]
          ;(println user-count)

;save-useremail-to-databes
          (postal/send-message 
             mail-config 
             {:from "pleasereply@remuse.co" 
             :to email
             :subject "Remuse welcomes you with open arms" 
             :body (str "Hello!  Thank you for joining the Remuse arstist, musician, listener community.  We send weekly emails that have all the latest track and album additions covered, you can opt-out any time.  We will only send you transactional emails about your account to this address, such as magic login links like this one. " link " If you can take a moment to reply and tell us how Remuse can help you make more money as a musician or a listener, send it over and tell us please!  Have an amazing day.")})
              (xt/submit-tx
                                               user/!xtdb ;;notice that in clj-land this is user/!xtdb as the "node"
                                               [[:xtdb.api/put
                                                 {:xt/id (random-uuid)
                                                  :user/id (ni)
                                                  :user/remewserid (str "RMWS|WSR|V000|"  user-count) ;; 0 for v1nc3nt, 2 can stay for listentowisdom710
                                                  :user/created-at in-ts
                                                  :user/created-by "vincent-stem-15july2023"
                                                  :user/modified-by "vincent-stem-15july2023"
                                                  :user/modified-at in-ts
                                                  :user/username "Fresh Music Lover"
                                                  :user/email email}]])

                                                  )))

(e/defn CreateAccountPage []
  (e/server
   (let [recs (e/offload #(user-records db))
         cust-count (count recs)]
    (e/client
     (let [success-payment (atom false)
           sp (e/watch success-payment)
           !sp1 (atom false)
           sp1 (e/watch !sp1)
           !sp2 (atom false)
           sp2 (e/watch !sp2)
           !sp3 (atom false)
           sp3 (e/watch !sp3)
           !cap-ins (atom nil)
           !w (e/watch !cap-ins)
           e (:user-email !w)
           h (:user-handle !w)
           f (:first-name !w)
           ln (:last-name !w)
           init (fn [] (.log js/console "hey stripe loaded x) jlyknw") (reset! !sl true))
           poll-stripe-loaded (fn [] (if (not (nil? js/Stripe)) (init) (js/setTimeout poll-stripe-loaded 100)))
           cmmp (.toFixed (+ 11.11 (* 0.33 cust-count)) 2)]
       (dom/div (dom/props {:class "rmws-mnll li"})
                (dom/table
                 (dom/tr (dom/th (dom/text "Music Lover / Musician")))
                 (dom/tr (dom/td (dom/strong (dom/text (str "$" cmmp "/yr*")))))
                 (dom/tr (dom/td (dom/text "Help make Remuse the best music discovery service in the world!")))
                 (dom/tr (dom/td (dom/text "Login via Magic Link")))
                 (dom/tr (dom/td (dom/text "Login Via Password")))
                 (dom/tr (dom/td (dom/text "Listening Party Chatroom Privileges")))
                 (dom/tr (dom/td (dom/text "Rate Tracks")))
                 (dom/tr (dom/td (dom/text "Rate Reviews")))
                 (dom/tr (dom/td (dom/text "Submit tracks to the review queue")))
                 (dom/tr (dom/td (dom/text "Fresh Music Remuse-letter (emailed)")))
                 (dom/tr (dom/td (dom/text "Become part of a lively community held together by the love of music."))))
                (dom/br)
                (dom/div (dom/props {:class "btc"})
                         (dom/span (dom/text "* Price increases by 33 cents every time a new person signs up.  You can learn more about our pricing model "))
                         (dom/a (dom/props {:href "https://remuser001.wordpress.com/2023/07/11/our-unique-pricing-model-at-remuse/"}) (dom/text "here"))
                         (dom/br)
                         (dom/div (dom/text "  The price you see on this screen is updated live."))
                         (dom/br)
                  ;(ui/button (e/fn [v] (e/server (inc-customer-count))) (dom/text "increase customer count"))

                         (dom/div (dom/props {:class ""}) (dom/text "Remuse is PCI-compliant."))
                         (poll-stripe-loaded)

                         ;(when (not (empty? sck))
                           (dom/div (dom/props {:class "peye"}) (dom/text "Please enter your email and payment info.  Request a login link once you have completed your purchase.  Your e-mail is your login."))
                  ;(ui/button (e/fn [v] (e/server (postal/send-message mail-config {:from "editor@remuse.co" :to "sova.kuliana@gmail.com" :subject "Remuse welcomes you with open arms" :body "Hello!  Thank you for joining the Remuse arstist, musician, listener community.  We send weekly emails that have all the latest track and album additions covered, you can opt-out any time.  We will only send you transactional emails about your account to this address, like reset password requests.  Have an amazing day."})))
                  ;  (dom/text "Send an e-mail to Vincent lol"))
                  ;(dom/div (dom/props {:class "sck"}) (dom/text sck))

                           (dom/div (dom/props {:class "ue"})
                                    (dom/div (dom/props {:class "scscsc"})
                                             (ui/input (get-in @!cap-ins :user-email) (e/fn [v] (swap! !cap-ins assoc :user-email v)) (dom/props {:placeholder "Enter your e-mail" :class ["ii" "ei"]})))
                      ;(ui/input "" (e/fn [v] (swap! !cap-ins assoc :user-handle v)) (dom/props {:placeholder "Enter your desired username" :class "ii"}))
                      ;(ui/input "" (e/fn [v] (swap! !cap-ins assoc :first-name v)) (dom/props {:placeholder "Enter your first name" :class "ii"}))
                      ;(ui/input "" (e/fn [v] (swap! !cap-ins assoc :last-name v)) (dom/props {:placeholder "Enter your last name" :class "ii"}))

                                    (dom/div (dom/props {:class "uee"}) (dom/text (str "Your e-mail: " e)))
                                    (dom/div (dom/props {:class "uee"}) (dom/text (str "Your membership price: " cmmp "/year"))))
                    ;(dom/div (dom/props {:class "uee"}) (dom/text (str "Desired username: " h)))
                    ;(dom/div (dom/props {:class "uee"}) (dom/text (str "First name: " f)))
                    ;(dom/div (dom/props {:class "uee"}) (dom/text (str "Last name: " ln)))

                           (PaymentForm.)
                           ;);closes when not empty? sck


                         (when sloaded
                           (init-stripe))
                         (when (empty? sck)
                           (ui/button
                            (e/fn []
                              (e/server
                               (let [rr (httpclient/post "https://api.stripe.com/v1/payment_intents"
                                                         {:basic-auth ["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" ""]
                                                          :form-params {"amount" (+ 1111 (* 33 cust-count))
                                                                        "currency" "usd"}})
                                     sr (:status rr)
                                     br (:body rr)
                                     cs (extract-client-secret br)]
                                ;(println "let's save the client secret which be " cs)
                                 (e/client
                                  (reset! !sck cs)))))
                                  ;;let's send the payment to stripe
                                  ;; grab the element (card element)
                                  ;;do the (.confirmCardPayment stripe client-secret)

                            (dom/props {:class ["ii" "lcaa"] :disabled (if (valid-email? e) false true)}) (dom/text "Let's Create an Account")))

                            (when (= "v1nc3ntpull1ng@gmail.com" (e/server (get-in e/*http-request* [:cookies "useremail" :value])))
                             (ui/button (e/fn [] (e/client (reset! !sp1 true) (.log js/console "set success payment"))) (dom/text "reste scp")))

                        (when (not sp)
                         (dom/div
                          (when (and sloaded (not (empty? sck)))
                           (ui/button
                            (e/fn []
                              (e/client
                               (let [S @stripe-instance
                                      ;elements (.elements S)
                                      ;cardElement (.create elements "card")
                                     card-elem (js/document.getElementById "card-element")
                                     s-promise-resp (.confirmCardPayment S sck (clj->js {:payment_method {:card @card-element-atom :billing_details {:email e}}}))]
                                 (.log js/console (str "attems confirm striape payeemenetm: " sck))
                                 (.then s-promise-resp
                                        (fn [res]
                                          (.log js/console "Payment Success")
                                          ;(reset! success-payment true)
                                          (reset! !sp1 true)
                                          ))
                                          ;;let's mint a new membership
                                          (.catch s-promise-resp (fn [erra] (.log js/console "erra: " erra))))))
                            (dom/props {:class "mmm"})
                            (dom/text (str "Mint my Membership!"))))))
                            
                            ;otherwise  success-payment
                          (when sp1
                            (dom/div 
                              (dom/text (str "Your purchase was successful.  Welcome to Remuse!  Please check your e-mail " e " for your inital login link."))
                              (e/server (mint-new-membership e))
                              (reset! !sp2 true)))
                          (when sp2
                            (dom/div 
                              (dom/text (str "Your purchase was successful and your account has been minted.  Welcome to Remuse!  Please check your e-mail " e " for your inital login link."))
                              (reset! success-payment "processed")
                              (reset! !sp1 false)))
                              )))))))

#?(:cljs (def client-message (atom "")))
#?(:cljs (e/def cm (e/watch client-message)))

(e/defn Todo-list []
  (e/server
   (binding [!xtdb user/!xtdb
             db (new (db/latest-db> user/!xtdb))]
     (e/client
      ;(.log js/console (get-current-path))
      ;(.log js/console "Current Definitons: " (str current-album-id xt-id))

      (case (get-current-path)  ;;set view dynamically based on url
        "/"                 (reset! !view :main-page)
        "/latest-additions" (reset! !view :latest-additions-page)
        "/settings"         (reset! !view :settings-page)
        "/logout"           (do (update-url "/") (reset! !view :main-page))
        "/authentik9"       (do (update-url "/") (reset! !view :main-page))
        "/create-account"   (reset! !view :create-account-page)
        (reset! !view :main-page))
      (dom/link (dom/props {:rel :stylesheet :href "/remews.css"}))
       ; (dom/script (dom/props {:src "https://sdk.scdn.co/spotify-player.js" :type "text/javascript"})) ;;spatifai no worky aac -_-;;;

    (dom/div (dom/props {:class "flex-contain" :id "remusetable"})
      (dom/div (dom/props {:class "flex-row"})
       (dom/div (dom/props {:class "bigR"}) (dom/div (dom/props {:id "bggR"}) (dom/div (dom/props {:class "lR flex-item"}) (dom/text "R E M U S E"))))
       (dom/div (dom/text ""))
        (when (e/server (get-in e/*http-request* [:cookies "remewserid" :value])) ;;signedonstats
         (dom/div (dom/props {:class "flex-item pr"}) 
         
                          (dom/div (dom/props {:class "bbbhhf"})
                           (dom/table (dom/tr 
                            (dom/td  (dom/props {:colspan 2 :class "liah"}) 
                              (dom/div 
                                (dom/text (str "" (e/server (get-in e/*http-request* [:cookies "useremail" :value])))))
                              (dom/div (dom/props {:class "bach"}) (dom/text "4 ⪏ 3"))
                              (dom/div (dom/props {:class ["sub" "fbl"]}) (ui/button (e/fn [] (e/client (reset! !view :settings-page) (update-url "settings"))) (dom/text "Settings") (dom/props {:class "subhu"}))))
                              
                            
                            
                            
                            )))
                            
                  ;else not signed in so
                    
                  ))
       (dom/div (dom/props {:class "flex-item"}))
       (when (not (e/server (get-in e/*http-request* [:cookies "remewserid" :value]))) ;;not signedon
         
         (dom/div (dom/props {:class "cranc flex-item flex-nogrow"}) 
           (ui/button (e/fn [] (e/client (reset! !view :create-account-page) (update-url "create-account"))) (dom/text "Create an Account") (dom/props {:class "subhu"}))
           (dom/div (dom/props {:class "log"}) (dom/a (dom/props {:class "loginbutan" :href "/magiclink.html"}) (dom/text "Login"))))))
      (dom/div (dom/props {:class "tr2 flex-row"})
       (dom/div (dom/props {:class "flex-item"}) (dom/text ""))
       (dom/div (dom/props {:class "flex-item"}) (dom/text ""))
       (dom/div (dom/props {:class "flex-item"}) (dom/text "")))
      
      ;;TOP|PREMUSE|SUBMIT with 2 flex-items abutted
      (dom/div (dom/props {:class "flex-row"}) 
       (dom/div (dom/props {:class "flex-item grow"})) 
       (dom/div (dom/props {:class "flex-item grow tps-btns"})
          (ui/button (e/fn [] (e/client (reset! !view :main-page) (update-url "/"))) (dom/text "TOP") (dom/props {:class ["subhut" "tps"]}))
          (ui/button (e/fn [] (e/client (reset! !view :preview-page) (update-url "preview"))) (dom/text "PREMUSE") (dom/props {:class ["subhut" "tps"]}))
          (ui/button (e/fn [] (e/client (reset! !view :convo-list-buttons) (update-url "discussions"))) (dom/text "DISCUSSIONS") (dom/props {:class ["subhut" "tps"]}))
          )
       (dom/div (dom/props (dom/props {:class "flex-item grow"}))))



       (dom/div (dom/props {:class "bbbhhh-left"})
          (dom/div (ui/button (e/fn [] (e/client (reset! !view :main-page) (update-url "/"))) (dom/props {:class "tiih"}) (dom/img (dom/props {:src "img/REMUSE.png"}))))
          ;(dom/div (dom/props {:class "htxt"}) (dom/text "REMUSE.co"))
          (dom/div (dom/props {:class "mo"}) (dom/text (str "👤 " (count present)))))

      (dom/div (dom/props {:id "bour" :class "flex-row"})
       (dom/div (dom/props {:class "fl"}) (dom/text ""))
       (dom/div (dom/props {:id "bou" :class "flex-column"}); aw yeah
        ;(when (= view :settings-page) (SettingsPage.)) ;;;just moving it up for a min to see if this will fix width issue
        
        (case view
                                   :track-list      (TrackList.)
                                   :user-list       (UserList.)
                                   :main-page       (MainPage.)
                                   :latest-additions-page (LatestAdditionsPage.)
                                   :create-account-page (CreateAccountPage.)
                                   :settings-page   (SettingsPage.) ;;;;moved up^ to maybe? fix width issue
                                   :preview-page    (PreviewPage.)
                                   :track-by-id     (TrackByIdPage. xt-id true)
                                   :project-by-name (ProjectByNamePage. current-project-name)
                                   :onvo-list       (OnvoList.) 
                                   :onvo-list-on-matching-convo-id (OnvoListOnMatchingConvoId.)
                                   :convo-list       (ConvoList.) 
                                   :convo-list-buttons (ConvoListButtons.)
                                   :onvo-list-on-matching-convo-id (OnvoListOnMatchingConvoId.)
                                   :inspect-db      (InspectDatabes.))

      (dom/div (dom/props {:class "bout"})
               (dom/div (dom/props {:class "c"})
                        (dom/div (dom/props {:class "bbhh"})
                                 ;(dom/div (dom/props {:class "bbh"})
                                 ;         (dom/div (dom/props {:class "mb"})))
                                 ;(dom/div (dom/props {:class "h"}) (dom/text "REMUSE")
                                 ;         (dom/div (dom/props {:class "i"}) (dom/text "listening parties and album reviews")))  
                                 
                                 ;;"admin"
                                 (when (= "v1nc3ntpull1ng@gmail.com" (e/server (get-in e/*http-request* [:cookies "useremail" :value])))
                                    (ui-nav-bttns.))

                                 
                                 ;(set! js/window.onSpotifyWebPlaybackSDKReady on-spotify-web-playback-sdk-ready)
                                 ;(dom/div (dom/props {:class "bbbfff"})
                                 ;   (dom/div
                                 ;    (dom/div (ui/button (e/fn []
                                 ;                          (e/client
                                 ;                           (reset! !view :main-page)
                                 ;                           (set-scroll-position 0 0)
                                 ;                           (update-url "/")))
                                 ;                        (dom/props {:class "tiih"})
                                 ;                        (dom/img (dom/props {:src "img/REMUSE.png"}))))
                                 ;    (dom/div (dom/props {:class "htxt"}) (dom/text "REMUSE.co"))
                                     
                                     ;(if (not (e/server (get-in e/*http-request* [:cookies "remewserid" :value])))
                                     ;  (do
                                     ;   (dom/div (dom/props {:class "sub"}) (ui/button (e/fn [] (e/client (reset! !view :create-account-page) (set-scroll-position 0 0) (update-url "create-account"))) (dom/text "Create an Account") (dom/props {:class "subhu"})))
                                     ;   (dom/div (dom/props {:class "httx"}) (dom/a (dom/props {:href "/login.html"}) (dom/text "Member Login"))))
                                     ;  (dom/div (dom/props {:class "sub"}) (ui/button (e/fn [] (e/client (reset! !view :settings-page) (set-scroll-position 0 0) (update-url "settings"))) (dom/text "Settings") (dom/props {:class "subhu"}))))
                                 ;       ))


                                 )))))
      (dom/div (dom/props {:class "flex-row"})
        (dom/div (dom/props {:class "flex-item"}) (dom/text ""))
        (dom/div (dom/props {:class "flex-item"}) (dom/div (dom/props {:class "devb" :id "fevb"})
          (dom/h2 (dom/span (dom/text "✉️")) (dom/text " Stay in the know"))
                                          (dom/div (dom/text "Remuse: fresh music discovery and community.  Get our newsletter for free and discover great tracks and new bands to love and cherish.  Premium members at Remuse.co can submit tracks (songs they made or simply songs they enjoy) to the Premuse page, eventually getting featured on the TOP page and in the Remuseletter.  Help us in our mission to help every band succeed, and to ensure that artists and musicians do not starve."))
                                          (dom/div
                                           (dom/span (dom/text "Create an account today, help "))
                                           (dom/span (dom/a (dom/props {:href "https://remuser001.wordpress.com/2023/07/09/on-the-problem-of-accelerating-music-discovery-remuses-mission/"})
                                                            (dom/text "our mission,")))
                                           (dom/span (dom/text " and lock in a great rate for an annual membership.")))
                                          (dom/div 
                                           (dom/span (dom/text "Join the 18 people who are already getting fresh music to their inbox! "))
                                           (dom/div (dom/props {:class "noboxshadow"})
                                             (dom/iframe
                                                      (dom/props
                                                       {:src "https://embeds.beehiiv.com/74f5f991-32b0-4713-88d8-7986fcfbe0fd?slim=true"
                                                        :data-test-id "beehiiv-embed"
                                                        :height "52"
                                                        :width "100%"
                                                        :frameborder "0"
                                                        :scrolling "no"
                                                        :class "bhsup"}))
                                                            ;(dom/a (dom/props {:class "bhiiv" :href "https://letter.remuse.co/p/r001"})
                                                            ;(dom/text "sign up for the twice-a-week premium newsletter"))
                                                            )
                                           ;(dom/span (dom/text "sign up for the free Fresh Music Remuseletter and get fresh music to your inbox."))
                                           )))
        (dom/div)))
                                           
                                           ) ;;/close .bout
   )) ;e/server
)


;;; new additions 6 may 2023
(e/defn TrackList []
  (dom/br)
  (TrackCreate.)
  (dom/div (dom/props {:class "track-list"})
           (dom/div {:class "track-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(track-records db))]
                               (TrackItem. id))))))

#?(:clj
   (defn track-records [db]
     (try (->> (xt/q db '{:find [(pull ?e [:xt/id :track/title :track/created-at])]
                     :where [[?e :track/title]
                             [?e :track/created-at created-at]]})
          (map first)
          (sort-by :track/created-at >)
          vec)
      (catch InterruptedException e))))

(e/defn TrackCreate []
  (e/client
   (InputSubmit.
    "enter a track title"
    (e/fn [v]
      (e/server
       (e/discard
        (xt/submit-tx
         !xtdb
         [[:xtdb.api/put
           {:xt/id (random-uuid)
            :track/id (nit)
            :track/title v
            :track/last-modified e/system-time-ms
            :track/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
            :track/created-at e/system-time-ms
            :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])}]])))))))

(e/defn TrackItem [id]
  (e/server
   (let [e (xt/entity db id)
         title (:track/title e)
         track-id (:track/id e)
         track-art (:track/art e)
         artist (:track/artist e)
         created-at (:track/created-at e)
         artist (:track/artist e)
         album-id (:track/album-id e)
         track-number (:track/number e)
         project-id (:track/project-id e)
         spotify-link (:track/spotify-link e)
         youtube-link (:track/youtube-link e)
         applemusic-link (:track/applemusic-link e)
         album (:track/album e)]
     (e/client
      (dom/div
       (dom/span (dom/text (str "Track title: " title)))
       (dom/br)
       (dom/span (dom/text (str " uuid: " id)))
       (dom/br)
       (dom/span (dom/text (str " created-at: " created-at)))
       (dom/br)
       (dom/span (dom/text (str "track ID: " track-id)))
       (dom/br)
       ;(dom/span (dom/text (str "associated album: " album)))
       ;(dom/br)
       ;(dom/span (dom/text (str "associated album-id: " album-id)))
       ;(dom/br)
       (dom/span (dom/text (str "artist: " artist)))
       (dom/br)
       (dom/span (dom/text (str "track number: " track-number)))
       (dom/br)
       (dom/span (dom/text (str "project-id: " project-id)))
       (dom/br)
       (dom/span (dom/text (str "spotify link: " spotify-link)))
       (dom/br)
       (dom/span (dom/text (str "yt link: " youtube-link)))
       (dom/br)
       (dom/span (dom/text (str "applemusic link: " applemusic-link)))
       (dom/br)
       (dom/span (dom/text (str "art: " track-art)))
       (dom/br)
       (InputSubmit. "title"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/album album
                                                :track/id (or track-id (nit))
                                                :track/album-id album-id
                                                :track/artist artist
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/project-id project-id
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/art track-art
                                                :track/title v}]])))))
       (InputSubmit. "track art"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/album album
                                                :track/id (or track-id (nit))
                                                :track/album-id album-id
                                                :track/artist artist
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/project-id project-id
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/title title
                                                :track/art v}]])))))
       (dom/span (dom/text (str album)))
       (InputSubmit. (str (when (empty? album-id) "a l b u m   i d ") album-id)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/title title
                                                :track/artist artist
                                                :track/id (or track-id (nit))
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/album album ;album
                                                ;(-> (xt/q db '{:find [?title]
                                                ;               :where [[?a :album/id album-id]
                                                ;                       [?a :album/title ?title]]
                                                ;               :in [album-id]}
                                                ;          album-id)
                                                ;    first first)
                                                :track/number track-number
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/project-id project-id
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/art track-art
                                                :track/album-id v}]])))))
       (InputSubmit. (str (when (empty? track-number) "t r a c k  #") track-number) ;;known issue :0
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/title title
                                                :track/artist artist
                                                :track/id (or track-id (nit))
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/album album
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/number (Integer/parseInt v)
                                                :track/project-id project-id
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/art track-art
                                                :track/album-id album-id}]])))))
       (InputSubmit. (str (when (empty? project-id) "p r o j e c t i d") project-id)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/title title
                                                :track/artist artist
                                                :track/id (or track-id (nit))
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/album album
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/number track-number
                                                :track/album-id album-id
                                                :track/art track-art
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/project-id v}]])))))
       (InputSubmit. (str (when (empty? artist) "a r t i s t ") artist)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/title title
                                                :track/artist v
                                                :track/id (or track-id (nit))
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/spotify-link spotify-link
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/project-id project-id
                                                :track/art track-art
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/album album}]])))))
       (InputSubmit. "spotify embed link"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/album album
                                                :track/id (or track-id (nit))
                                                :track/album-id album-id
                                                :track/artist artist
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/project-id project-id
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link applemusic-link
                                                :track/art track-art
                                                :track/spotify-link v
                                                :track/title title}]])))))
                                                
      (InputSubmit. "youtube embed link"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/album album
                                                :track/id (or track-id (nit))
                                                :track/album-id album-id
                                                :track/artist artist
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/project-id project-id
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/applemusic-link applemusic-link
                                                :track/youtube-link v
                                                :track/art track-art
                                                :track/spotify-link spotify-link
                                                :track/title title}]])))))
        (InputSubmit. "apple music everything after the /"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :track/album album
                                                :track/id (or track-id (nit))
                                                :track/album-id album-id
                                                :track/artist artist
                                                :track/last-modified e/system-time-ms
                                                :track/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :track/number track-number
                                                :track/project-id project-id
                                                :track/created-at (or created-at e/system-time-ms)
                                                :track/youtube-link youtube-link
                                                :track/applemusic-link v
                                                :track/art track-art
                                                :track/spotify-link spotify-link
                                                :track/title title}]]))))))))))

(e/defn UserItem [id]
  (e/server
   (let [e (xt/entity db id)
         xt-id (:xt/id e)
         user-id (:user/id e)
         username (:user/username e)
         review-ids (:user/review-ids e)
         rating-ids (:user/rating-ids e)
         ratings (:user/ratings e)
         email (:user/email e)
         img (:user/img e)
         created-at (:user/created-at e)
         created-by (:user/created-by e)
         user-title (:user/title e)
         last-modified (:user/modified-at e)
         modified-by (:user/modified-by e)
         user-password (:user/password e)
         remewserid (:user/remewserid e)]
     (e/client
      (dom/div
       (dom/table (dom/props {:class "ute"})
                  (dom/tr
                   (dom/td (dom/text (str "Useremail " email)))
                   (dom/td (dom/text (str "Username " username)))
                   (dom/td (dom/text (str "xt-id " xt-id)))
                   (dom/td (dom/text (str "usertitle: " user-title)))
                   (dom/td (dom/text (str "Userpass " user-password))))
                  (dom/tr
                   (dom/td (dom/text (str "created-by: " created-by)))
                   (dom/td (dom/text (str "remewser: " remewserid)))
                   (dom/td (dom/text (str "Modified by: " modified-by)))
                   (dom/td (dom/text (str "last modified: " last-modified)))))
       (dom/br)
       (InputSubmit. "Usertitle"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :user/id (or user-id (ni))
                                                :user/last-modified e/system-time-ms
                                                :user/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :user/title v
                                                :user/remewserid remewserid
                                                :user/username username
                                                :user/password user-password
                                                :user/email email}]])))))
        (InputSubmit. "remewserid"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :user/id (or user-id (ni))
                                                :user/last-modified e/system-time-ms
                                                :user/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :user/title user-title
                                                :user/username username
                                                :user/password user-password
                                                :user/email email
                                                :user/remewserid v}]])))))

       (InputSubmit. "Useremail"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :user/id (or user-id (ni))
                                                :user/last-modified e/system-time-ms
                                                :user/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :user/title user-title
                                                :user/username username
                                                :user/remewserid remewserid
                                                :user/password user-password
                                                :user/email v}]])))))
       (InputSubmit. "Username"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :user/id (or user-id (ni))
                                                :user/last-modified e/system-time-ms
                                                :user/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :user/email email
                                                :user/title user-title
                                                :user/remewserid remewserid
                                                :user/password user-password
                                                :user/username v}]]))))))))))

(e/defn UserList []
  (dom/div (dom/props {:class "user-list"})
           ;(UserCreate.)
           (dom/div (dom/props {:class "user-items"})
                    (e/server
                     (let [recs (e/offload #(user-records db))
                           c (count recs)]
                       (e/client (dom/div (dom/props {:class "uc"}) (dom/text (str "#U " c))))
                      (e/for-by :xt/id [{:keys [xt/id]} recs]
                                (UserItem. id)))))))

#?(:clj
   (defn latest-addition-track-records [db]
     (try 
      (->> (xt/q db '{:find [(pull ?t [:xt/id :track/id :track/title :track/created-at])]
                     :where [[?t :track/id id]]})
           (map first)
           (sort-by :track/created-at >)
           (take 9)
           vec)
     (catch InterruptedException e))))

#?(:clj
   (defn tracks-fo-project [db project-name]
     (try 
      (->> (xt/q db '{:find [(pull ?track [:xt/id :track/id :track/title :track/created-at])]
                     :where [[?track :track/artist artist-name]]
                     :in [artist-name]}
                     project-name)
           (map first)
           (sort-by :track/created-at >)
           vec)
     (catch InterruptedException e))))

;;; new additions 2 june 2023
(e/defn InspectDatabes []
  (dom/br)
  (dom/div (dom/props {:class "track-list"})
           (dom/div {:class "track-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(all-db-records db))]
                               (InspectItem. id))))))

#?(:clj
   (defn all-db-records [db]
     (try 
      (->> (xt/q db '{:find [(pull ?e [:xt/id])]
                      ;:where [[?e :rating/id]]
                      :where [[?e :xt/id]]
                      })
                              ;[?e :rating/id rating-id] ;;just ratings
           (map first)
           vec)
     (catch InterruptedException e))))

(e/defn InspectItem [id]
  (e/server
   (let [e (xt/entity db id)
         xt-id (:xt/id e)]
     (e/client
      (dom/div
       (dom/span (dom/text (str "xt-id: " xt-id)))
       (dom/br)
       ;(InputSubmit. (str "delete " xt-id "?")
       (ui/button
        (e/fn [v]
          (e/server
           (e/discard
            (xt/submit-tx !xtdb [[:xtdb.api/delete xt-id]]))))
        (dom/text xt-id)))))))

(e/defn SettingsPage []
  (e/server
   (let [current-remewser (get-in e/*http-request* [:cookies "remewserid" :value])]
     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(the-mine-user-record db (str current-remewser)))]
       (let [r (xt/entity db id)
             created-at (:user/created-at r)
             user-id (:user/id r)
             user-email (:user/email r)
             username (:user/username r)
             password (:user/password r) ; chek if pas maken porblam
             ;password ""
             ]
         (e/client 
      (dom/div
       (dom/props {:class "userbio"})
       ;(dom/a (dom/props {:href "/" :class "alabama"}) (dom/text "home"))
       (ui/button
        (e/fn [] (e/client (do (update-url "/") (reset! !view :main-page))))
        (dom/text "Back to Main Page")
        (dom/props {:class "alabama"}))
       (dom/a (dom/props {:href "/logout" :class "florida"}) (dom/text "Logout"))
       ;(dom/div (dom/text (str "current remewserid " current-remewser))) ;;hide this
       ;(dom/div (dom/text r))
       ;(dom/div (dom/text "Added at " created-at)) ;;hide this
       (dom/br)
       (dom/div (dom/text "email: " user-email))
       (dom/div (dom/text "user-id " user-id))
       (dom/div (dom/text "current username: " (or username "New Member")))
       (dom/br)
       (when (or (not password) (= "" password))
         (InputSubmit.
          "Set a Password"
          (e/fn [v]
            (e/server
             (e/discard
              (xt/submit-tx
               !xtdb
               [[:xtdb.api/put
                 {:xt/id id
                  :user/id user-id
                  :user/remewserid current-remewser
                  :user/created-at created-at
                  :user/modified-at e/system-time-ms
                  :user/created-by "vincent-stem-15july2023"
                  :user/modified-by "settings-stem-17july2023"
                  :user/username username
                  :user/password (hash-with :argon2 v)
                  :user/email user-email}]]))))))
       (when (and password (not= password ""))
         (ui/button 
          (e/fn [] 
            (e/server 
             (e/discard
              (xt/submit-tx
               !xtdb
               [[:xtdb.api/put
                 {:xt/id id
                  :user/id user-id
                  :user/remewserid current-remewser
                  :user/created-at created-at
                  :user/modified-at e/system-time-ms
                  :user/created-by "vincent-stem-15july2023"
                  :user/modified-by "settings-stem-17july2023"
                  :user/username username
                  :user/password ""
                  :user/email user-email}]]))))
          (dom/text "Reset Password")))
       (dom/br)

       (InputSubmit.
        "Desired Username"
        (e/fn [v]
          (e/server
           (e/discard
            (xt/submit-tx
             !xtdb
             [[:xtdb.api/put
               {:xt/id id
                :user/id user-id
                :user/remewserid current-remewser
                :user/created-at created-at
                :user/modified-at e/system-time-ms
                :user/created-by "vincent-stem-15july2023"
                :user/modified-by "settings-stem-17july2023"
                :user/password password
                :user/username v
                :user/email user-email}]])))))
       ;(dom/div (dom/text "Set Password"))
       ;(dom/div (dom/text "Reset Password"))
       (dom/br)
       ;(dom/a (dom/props {:href "mailto:subscriptions@remuse.co"}) (dom/text "subscriptions@remuse.co"))
       (dom/br)
       (dom/div (dom/props {:class "chat-ext-hidden"}) (ChatExtended.)))))))))





;;TrackItem -> PreviewItem :check:

;;LatestAdditionTrackItemWithChat -> PreviewPageWithChat


#?(:clj
   (defn preview-records-all [db]
     (try (->> (xt/q db '{:find [(pull ?p [:xt/id :preview/title :preview/created-at])]
                     :where [[?p :preview/title]
                             [?p :preview/created-at created-at]]})
          (map first)
          (sort-by :preview/created-at >)
          vec)
      (catch InterruptedException e))))

#?(:clj
   (defn preview-records-verified [db]
     (try (->> (xt/q db '{:find [(pull ?p [:xt/id :preview/title :preview/created-at])]
                     :where [[?p :preview/status in-status]
                             [?p :preview/created-at created-at]]
                     :in [in-status]}
                     "verified")
          (map first)
          (sort-by :preview/created-at >)
          vec)
      (catch InterruptedException e))))

#?(:clj
   (defn preview-records-metadata-added [db]
     (try (->> (xt/q db '{:find [(pull ?p [:xt/id :preview/title :preview/created-at])]
                     :where [[?p :preview/status in-status]
                             [?p :preview/created-at created-at]]
                     :in [in-status]}
                     "metadata-added")
          (map first)
          (sort-by :preview/created-at >)
          vec)
      (catch InterruptedException e))))

#?(:clj
   (defn preview-records-image-added [db]
     (try (->> (xt/q db '{:find [(pull ?p [:xt/id :preview/title :preview/created-at])]
                     :where [[?p :preview/status in-status]
                             [?p :preview/created-at created-at]]
                     :in [in-status]}
                     "image-added")
          (map first)
          (sort-by :preview/created-at >)
          vec)
      (catch InterruptedException e))))

#?(:clj
   (defn preview-records-just-added [db]
     (try (->> (xt/q db '{:find [(pull ?p [:xt/id :preview/title :preview/created-at])]
                     :where [[?p :preview/status in-status]
                             [?p :preview/created-at created-at]]
                     :in [in-status]}
                     "just-added")
          (map first)
          (sort-by :preview/created-at >)
          vec)
      (catch InterruptedException e))))

(e/defn PreviewPage [] ;not previewList which is for adminsze
 (dom/table (dom/props {:id "premusetable"})
 (dom/tr 
  (dom/td (dom/props {:class "plv1"}) (dom/text "Verified Entries")))
  ;(dom/div (dom/props {:class ["preview-list" "plv"]})
  ;         (dom/div {:class "preview-items"}
  (dom/tr
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(preview-records-verified db))]
                                (let [title-edit? false
                                      artist-edit? false
                                      album-edit? false
                                      yt-edit? false
                                      sp-edit? false 
                                      am-edit? false
                                      artwork-edit? false
                                      track-number-edit? false]
                               (PreviewDisplayItem. id title-edit? artist-edit? album-edit? yt-edit? sp-edit? am-edit? artwork-edit? track-number-edit?)))))
                               ;)

  (dom/td (dom/props {:class "plma1"}) (dom/text "Metadata Recently Added"))
  ;(dom/div (dom/props {:class ["preview-list" "plma"]})
   ;        (dom/div {:class "preview-items"}
                    (e/server
                    (let [title-edit? true
                                      artist-edit? true
                                      album-edit? true
                                      yt-edit? false
                                      sp-edit? false 
                                      am-edit? false
                                      artwork-edit? false
                                      track-number-edit? true]
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(preview-records-metadata-added db))]
                             
                               (PreviewDisplayItem. id title-edit? artist-edit? album-edit? yt-edit? sp-edit? am-edit? artwork-edit? track-number-edit?))))

(dom/tr
  (dom/td (dom/props {:class ["plia1"]}) (dom/text "Image Recently Added")))
(dom/tr
  (e/server
  (let [title-edit? false
        artist-edit? false
        album-edit? false
        yt-edit? true
        sp-edit? true 
        am-edit? true
        artwork-edit? false
        track-number-edit? true]
    (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(preview-records-image-added db))]
      (PreviewDisplayItem. id title-edit? artist-edit? album-edit? yt-edit? sp-edit? am-edit? artwork-edit? track-number-edit?)))))

  
(dom/tr
  (dom/td (dom/props {:class ["plja1"]}) (dom/text "Just Added")))
(dom/tr
  (e/server
    (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(preview-records-just-added db))]
              (let [title-edit? true
                    artist-edit? true
                    album-edit? false
                    yt-edit? false
                    sp-edit? false 
                    am-edit? false
                    artwork-edit? true
                    track-number-edit? false]
              (PreviewDisplayItem. id true artist-edit? album-edit? yt-edit? sp-edit? am-edit? artwork-edit? track-number-edit?)))))
                               
  (dom/tr (dom/props {:class "precre"})
    (when (not (e/server (get-in e/*http-request* [:cookies "remewserid" :value]))) ;;not signedon
      (dom/td (dom/props {:class "bgw"}) (dom/text "Please log in to add tracks to the Premuse page.")))
    (PreviewCreate.))))
      
(e/defn PreviewList []
  (dom/br)
  (PreviewCreate.)
  (dom/div (dom/props {:class "preview-list"})
           (dom/div {:class "preview-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(preview-records-all db))]
                               (PreviewItem. id))))))

(e/defn PreviewCreate []
 (e/client
  (dom/div (dom/props {:class "pc"})
   (InputSubmit.
    "enter all relevant track info (artist, track, specific performance date if relevant)"
    (e/fn [v]
      (e/server
       (e/discard
        (xt/submit-tx
         !xtdb
         [[:xtdb.api/put
           {:xt/id (random-uuid)
            :preview/id (nit)
            :preview/title v
            :preview/status "just-added"
            :preview/last-modified e/system-time-ms
            :preview/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
            :preview/created-at e/system-time-ms
            :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])}]]))))))))

(e/defn PreviewItem [id]
  (e/server
   (let [e (xt/entity db id)
         title (:preview/title e)
         track-id (:preview/id e)
         track-art (:preview/art e)
         artist (:preview/artist e)
         created-at (:preview/created-at e)
         artist (:preview/artist e)
         album-id (:preview/album-id e)
         track-number (:preview/number e)
         project-id (:preview/project-id e)
         spotify-link (:preview/spotify-link e)
         youtube-link (:preview/youtube-link e)
         applemusic-link (:preview/applemusic-link e)
         album (:preview/album e)
         status (:preview/status e)]
     (e/client
      (dom/div
       (dom/span (dom/text (str "Status: " status)))
       (dom/br)
       (dom/span (dom/text (str "Track title: " title)))
       (dom/br)
       (dom/span (dom/text (str " uuid: " id)))
       (dom/br)
       (dom/span (dom/text (str " created-at: " created-at)))
       (dom/br)
       (dom/span (dom/text (str "track ID: " track-id)))
       (dom/br)
       ;(dom/span (dom/text (str "associated album: " album)))
       ;(dom/br)
       ;(dom/span (dom/text (str "associated album-id: " album-id)))
       ;(dom/br)
       (dom/span (dom/text (str "artist: " artist)))
       (dom/br)
       (dom/span (dom/text (str "track number: " track-number)))
       (dom/br)
       (dom/span (dom/text (str "project-id: " project-id)))
       (dom/br)
       (dom/span (dom/text (str "spotify link: " spotify-link)))
       (dom/br)
       (dom/span (dom/text (str "yt link: " youtube-link)))
       (dom/br)
       (dom/span (dom/text (str "applemusic link: " applemusic-link)))
       (dom/br)
       (dom/span (dom/text (str "art: " track-art)))
       (dom/br)
       (InputSubmit. "title"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/title v}]])))))
       (InputSubmit. "track art"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/title title
                                                :preview/status status
                                                :preview/art v}]])))))
       (dom/span (dom/text (str album)))
       (InputSubmit. (str (when (empty? album-id) "a l b u m   i d ") album-id)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist artist
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/album album ;album
                                                :preview/number track-number
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/album-id v}]])))))
       (InputSubmit. (str (when (empty? track-number) "t r a c k  #") track-number) ;;known issue :0
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist artist
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/album album
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/number (Integer/parseInt v)
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/album-id album-id}]])))))
       (InputSubmit. (str (when (empty? project-id) "p r o j e c t i d") project-id)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist artist
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/album album
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/number track-number
                                                :preview/album-id album-id
                                                :preview/art track-art
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/status status
                                                :preview/project-id v}]])))))
       (InputSubmit. (str (when (empty? artist) "a r t i s t ") artist)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist v
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/project-id project-id
                                                :preview/art track-art
                                                :preview/album-id album-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/status status
                                                :preview/album album}]])))))
       (InputSubmit. "spotify embed link"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/art track-art
                                                :preview/spotify-link v
                                                :preview/status status
                                                :preview/title title}]])))))
                                                
      (InputSubmit. "youtube embed link"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/applemusic-link applemusic-link
                                                :preview/youtube-link v
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status status
                                                :preview/title title}]])))))
        (InputSubmit. "apple music everything after the /"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link v
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status status
                                                :preview/title title}]])))))
                                                
          (InputSubmit. "status"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/applemusic-link applemusic-link
                                                :preview/youtube-link youtube-link
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status v
                                                :preview/title title}]]))))))))))

(e/defn PreviewDisplayItem [id title-edit? artist-edit? album-edit? yt-edit? sp-edit? am-edit? artwork-edit? track-number-edit?]
  (e/server
   (let [e (xt/entity db id)
         title (:preview/title e)
         track-id (:preview/id e)
         track-art (:preview/art e)
         artist (:preview/artist e)
         created-at (:preview/created-at e)
         artist (:preview/artist e)
         album-id (:preview/album-id e)
         track-number (:preview/number e)
         project-id (:preview/project-id e)
         spotify-link (:preview/spotify-link e)
         youtube-link (:preview/youtube-link e)
         applemusic-link (:preview/applemusic-link e)
         album (:preview/album e)
         status (:preview/status e)]
     (e/client
      (dom/td (dom/props {:class "monopolcard-p"})
       (dom/img (dom/props {:class "pimg" :src (if track-art (str "img/" track-art) (str "img/R-beam-awaiting-art.png"))}))
       (dom/br)
       (dom/div (dom/props {:class "mc-details"})
      ; (dom/span (dom/text (str "Status: " status)))
      ; (dom/br)
      ; (dom/span (dom/text (str "Track title: " title)))
      ; (dom/br)
      ; (dom/span (dom/text (str " uuid: " id)))
      ; (dom/br)
      ; (dom/span (dom/text (str " created-at: " created-at)))
      ; (dom/br)
      ; (dom/span (dom/text (str "track ID: " track-id)))
      ; (dom/br)
      ; (dom/span (dom/text (str "artist: " artist)))
      ; (dom/br)
      ; (dom/span (dom/text (str "track number: " track-number)))
      ; (dom/br)
      ; (dom/span (dom/text (str "project-id: " project-id)))
      ; (dom/br)
       (when spotify-link
         (dom/div (dom/text (str "https://open.spotify.com/track/" spotify-link))))
       ;(dom/br)
       (when youtube-link
         (dom/div (dom/text (str "https://www.youtube.com/watch?v=" youtube-link))))
       ;(dom/br)
       (when applemusic-link
         (dom/div (dom/text (str "https://music.apple.com/" applemusic-link))))
       ;(dom/br)
       (dom/div (dom/text (str title " - " artist )))
       
       ;(dom/span (dom/text (str "art: " track-art)))
       (dom/br))
       (when title-edit?
       (AdminInputSubmit. "title"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/title v}]]))))))
      (when artwork-edit?                                         
       (AdminInputSubmit. "track art"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/title title
                                                :preview/status status
                                                :preview/art v}]]))))))
       (dom/span (dom/text (str album)))
       (when album-edit? 
       (AdminInputSubmit. (str (when (empty? album-id) "a l b u m   i d ") album-id)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist artist
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/album album ;album
                                                :preview/number track-number
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/album-id v}]]))))))

    (when track-number-edit?   
       (AdminInputSubmit. (str (when (empty? track-number) "t r a c k  #") track-number) ;;known issue :0
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist artist
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/album album
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/number (Integer/parseInt v)
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/art track-art
                                                :preview/status status
                                                :preview/album-id album-id}]]))))))
      (when artist-edit?
       (AdminInputSubmit. (str (when (empty? artist) "a r t i s t ") artist)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/title title
                                                :preview/artist v
                                                :preview/id (or track-id (nit))
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/spotify-link spotify-link
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/project-id project-id
                                                :preview/art track-art
                                                :preview/album-id album-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/status status
                                                :preview/album album}]]))))))
      (when sp-edit? 
       (AdminInputSubmit. (str (when (empty? spotify-link) "spotify embed link") spotify-link)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link applemusic-link
                                                :preview/art track-art
                                                :preview/spotify-link v
                                                :preview/status status
                                                :preview/title title}]]))))))

    (when yt-edit?
      (AdminInputSubmit. (str (when (empty? youtube-link) "youtube embed link") youtube-link)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/applemusic-link applemusic-link
                                                :preview/youtube-link v
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status status
                                                :preview/title title}]]))))))
      (when   am-edit?                                        
        (AdminInputSubmit. (str (when (empty? applemusic-link) "apple music everything after the /") applemusic-link)
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/youtube-link youtube-link
                                                :preview/applemusic-link v
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status status
                                                :preview/title title}]]))))))
                                                
          (AdminInputSubmit. "status"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :preview/album album
                                                :preview/id (or track-id (nit))
                                                :preview/album-id album-id
                                                :preview/artist artist
                                                :preview/last-modified e/system-time-ms
                                                :preview/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :preview/number track-number
                                                :preview/project-id project-id
                                                :preview/created-at (or created-at e/system-time-ms)
                                                :preview/applemusic-link applemusic-link
                                                :preview/youtube-link youtube-link
                                                :preview/art track-art
                                                :preview/spotify-link spotify-link
                                                :preview/status v
                                                :preview/title title}]]))))))))))

;;; new additions 26 sept 2023
(e/defn OnvoList []
  (dom/br)
  (OnvoCreate.)
  (dom/div (dom/props {:class "onvo-list"})
           (dom/div {:class "onvo-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(onvo-records db))]
                               (OnvoItem. id))))))

#?(:clj
   (defn onvo-records [db]
     (try (->> (xt/q db '{:find [(pull ?o [:xt/id :onvo/title :onvo/created-at])]
                     :where [[?o :onvo/title]
                             [?o :onvo/created-at created-at]]})
          (map first)
          (sort-by :onvo/created-at >)
          vec)
      (catch InterruptedException e))))

(e/defn OnvoCreate []
  (e/client
   (InputSubmit.
    "enter an onvo title"
    (e/fn [v]
      (e/server
       (e/discard
        (xt/submit-tx
         !xtdb
         [[:xtdb.api/put
           {:xt/id (random-uuid)
            :onvo/id (nit)
            :onvo/title v
            :onvo/last-modified e/system-time-ms
            :onvo/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
            :onvo/created-at e/system-time-ms
            :onvo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])}]])))))))

(e/defn OnvoItem [id]
  (e/server
   (let [o (xt/entity db id)
         title       (:onvo/title o)
         onvo-id     (:onvo/id o)
         convo-id    (:onvo/convo-id o)
         created-at  (:onvo/created-at o)
         votes-to-promote-to-question (:onvo/votes-to-promote-to-question o)
         votes-to-promote-to-answer   (:onvo/votes-to-promote-to-answer o)
         votes-to-promote-to-topic    (:onvo/votes-to-promote-to-topic o) ;;topic is synonymous with convo in this context. 
         modified-by (:onvo/modified-by o)
         xt-id       (:xt/id o)]
     (e/client
      (dom/div
       (dom/div (dom/props {:class "flcl"}) (dom/text (str "Onvo title: " title)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " uuid: " xt-id)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " created-at: " created-at)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " modified-by: " modified-by)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str "onvo ID: " onvo-id)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str "convo id: " convo-id)))
       (dom/br)
       (InputSubmit. "onvo title"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :onvo/id (or onvo-id (nir))
                                                :onvo/convo-id convo-id 
                                                :onvo/last-modified e/system-time-ms
                                                :onvo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :onvo/created-at (or created-at e/system-time-ms)
                                                :onvo/title v
                                                :onvo/votes-to-promote-to-question votes-to-promote-to-question
                                                :onvo/votes-to-promote-to-answer votes-to-promote-to-answer
                                                :onvo/votes-to-promote-to-topic votes-to-promote-to-topic}]])))))
       (InputSubmit. "onvo/convo-id"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :onvo/id (or onvo-id (nir))
                                                :onvo/convo-id v
                                                :onvo/last-modified e/system-time-ms
                                                :onvo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :onvo/created-at (or created-at e/system-time-ms)
                                                :onvo/title title
                                                :onvo/votes-to-promote-to-question votes-to-promote-to-question
                                                :onvo/votes-to-promote-to-answer votes-to-promote-to-answer
                                                :onvo/votes-to-promote-to-topic votes-to-promote-to-topic}]]))))))))))

(e/defn ConvoList []
  (dom/br)
  (ConvoCreate.)
  (dom/div (dom/props {:class "convo-list"})
           (dom/div {:class "convo-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(convo-records db))]
                               (ConvoItem. id))))))

#?(:clj
   (defn convo-records [db]
     (try (->> (xt/q db '{:find [(pull ?o [:xt/id :convo/title :convo/id :convo/created-at])]
                     :where [[?o :convo/title]
                             [?o :convo/created-at created-at]]})
          (map first)
          (sort-by :convo/created-at >)
          vec)
      (catch InterruptedException e))))

(e/defn ConvoCreate []
  (e/client
   (InputSubmit.
    "enter an convo title"
    (e/fn [v]
      (e/server
       (e/discard
        (xt/submit-tx
         !xtdb
         [[:xtdb.api/put
           {:xt/id (random-uuid)
            :convo/id (nit)
            :convo/title v
            :convo/last-modified e/system-time-ms
            :convo/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
            :convo/created-at e/system-time-ms
            :convo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])}]])))))))

(e/defn ConvoItem [id]
  (e/server
   (let [o (xt/entity db id)
         title       (:convo/title o)
         convo-id    (:convo/id o)
         created-at  (:convo/created-at o)
         convo-status (:convo/status o)
         votes-to-promote-to-question (:convo/votes-to-promote-to-question o)
         ;votes-to-promote-to-answer  (:onvo/votes-to-promote-to-answer o) ;;in theory can no longer be an answer if it is a topic or question o_O right? lol i think so.
         votes-to-promote-to-topic    (:convo/votes-to-promote-to-topic o) ;;topic is synonymous with convo in this context. 
         modified-by (:convo/modified-by o)
         xt-id       (:xt/id o)]
     (e/client
      (dom/div
       (dom/div (dom/props {:class "flcl"}) (dom/text (str "Convo title: " title)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " uuid: " xt-id)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " created-at: " created-at)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str " modified-by: " modified-by)))
       (dom/div (dom/props {:class "flcl"}) (dom/text (str "convo id: " convo-id)))
       (dom/br)
       (InputSubmit. "convo title"
                     (e/fn [v]
                       (e/server
                        (e/discard
                         (xt/submit-tx !xtdb [[:xtdb.api/put
                                               {:xt/id id
                                                :convo/id (or convo-id (nit))
                                                :convo/convo-id convo-id 
                                                :convo/last-modified e/system-time-ms
                                                :convo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])
                                                :convo/created-at (or created-at e/system-time-ms)
                                                :convo/title v
                                                :convo/votes-to-promote-to-question votes-to-promote-to-question
                                                ;:convo/votes-to-promote-to-answer votes-to-promote-to-answer
                                                :convo/votes-to-promote-to-topic votes-to-promote-to-topic

                                                ;; TOPIC | QUESTION | ANSWER (topic is synonymous with convo.)
                                                :convo/status (or convo-status "topic")}]]))))))))))


(e/defn ConvoListButtons []
  (dom/div (dom/props {:class "convobuttons-list"})
           (dom/div {:class "convobuttons-items"}
                    (e/server
                     (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(convo-records db))]
                               (ConvoButton. id))))))


(e/defn ConvoButton [id]
  (e/server
   (let [o (xt/entity db id)
         title       (:convo/title o)
         convo-id    (:convo/id o)
         created-at  (:convo/created-at o)
         convo-status (:convo/status o)
         votes-to-promote-to-question (:convo/votes-to-promote-to-question o)
         votes-to-promote-to-topic    (:convo/votes-to-promote-to-topic o) ;;topic is synonymous with convo in this context. 
         modified-by (:convo/modified-by o)
         xt-id       (:xt/id o)]
     (e/client
      (dom/div
       (dom/div (dom/props {:class "flcl"}) 
         (ui/button (e/fn [] (reset! !view :onvo-list-on-matching-convo-id)
                             (reset! !current-convo-id convo-id) 
                             (.log js/console convo-id)
                             (reset! !current-convo-title (str title)))
            (dom/props {:class "convobutan"})
            (dom/text (str "" title)))))))))


            

(e/defn OnvoListOnMatchingConvoId []
 (let [cci current-convo-id]
  (dom/div (dom/props {:class "onvo-list-on-matching-convo-id flex-column"})
    (dom/div (dom/props {:class "gobak flex-item"}) 
      (ui/button (e/fn [] (e/client (reset! !view :convo-list-buttons)))
        (dom/props {:class "convobutan"})
        (dom/text "⬅️"))
      (dom/div (dom/props {:class "ctitle"}) (dom/text current-convo-title))) 
    (dom/div {:class "onvo-items-on-matching-convo-id"}
      (e/server
        (e/for-by :xt/id [{:keys [xt/id]} (e/offload #(onvo-for-matching-convo-id-records db cci))]
          (OnvoItemOnMatchingConvoId. id))))
    (OnvoCreateOnMatchingConvoId.))))

#?(:clj
   (defn onvo-for-matching-convo-id-records [db cci]
     (try (->> (xt/q db '{:find [(pull ?o [:xt/id :onvo/title :onvo/created-at])]
                     :where [[?o :onvo/title]
                             [?o :onvo/convo-id conversation-or-topic-id]
                             [?o :onvo/created-at created-at]]
                     :in [conversation-or-topic-id]}
                  cci)
          (map first)
          (sort-by :onvo/created-at <)
          vec)
      (catch InterruptedException e))))

(e/defn OnvoCreateOnMatchingConvoId []
  (e/client
   (let [cci current-convo-id
         cct current-convo-title]
   (dom/div (dom/props {:class "flcl"})
   (InputSubmit.
    "enter a respectful msg"
    (e/fn [v]
      (e/server
       (e/discard
        (xt/submit-tx
         !xtdb
         [[:xtdb.api/put
           {:xt/id (random-uuid)
            :onvo/convo-id cci
            :onvo/convo-title cct
            :onvo/id (nit)
            :onvo/title v
            :onvo/last-modified e/system-time-ms
            :onvo/created-by (get-in e/*http-request* [:cookies "remewserid" :value])
            :onvo/created-at e/system-time-ms
            :onvo/modified-by (get-in e/*http-request* [:cookies "remewserid" :value])}]])))))))))

(e/defn OnvoItemOnMatchingConvoId [id]
  (e/server
   (let [o (xt/entity db id)
         title       (:onvo/title o)
         onvo-id     (:onvo/id o)
         convo-id    (:onvo/convo-id o)
         convo-title (:onvo/convo-title o)
         created-at  (:onvo/created-at o)
         votes-to-promote-to-question (:onvo/votes-to-promote-to-question o)
         votes-to-promote-to-answer   (:onvo/votes-to-promote-to-answer o)
         votes-to-promote-to-topic    (:onvo/votes-to-promote-to-topic o) ;;topic is synonymous with convo in this context. 
         modified-by (:onvo/modified-by o)
         xt-id       (:xt/id o)]
     (e/client
      (dom/div
       (dom/div (dom/props {:class "flcl chatline"}) (dom/text (str (e/server (get-username db modified-by)) ": " title))))))))

#?(:clj
   (defn get-username [db remewser-id]
     (try
       (->> (xt/q db '{:find [(pull ?e [:xt/id :user/username :user/remewserid])]
                     :where [[?e :user/remewserid crid]]
                     :in [crid]}
                remewser-id) ;"RMWS|WSR|V000|7"
          (map first)
          vec
          first
          :user/username)
      (catch InterruptedException e))))