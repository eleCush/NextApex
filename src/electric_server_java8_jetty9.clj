;; Start from this example if you need Java 8 compat.
;; See `deps.edn`
(ns electric-server-java8-jetty9
  (:require [clojure.java.io :as io]
            [hyperfiddle.electric-jetty-adapter :as adapter]
            [clojure.tools.logging :as log]
            [ring.adapter.jetty9 :as ring]
            [ring.middleware.basic-authentication :as auth]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.json :as json]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as res]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [app.xtdb-contrib :as db]
            [xtdb.api :as xt]
            [postal.core :as postal]
            [cryptohash-clj.api :as ch :refer [hash-with verify-with]])
  (:import [java.io IOException]
           [java.net BindException]
           [org.eclipse.jetty.server.handler.gzip GzipHandler]))

(defn authenticate [username password] 
  username) ; demo (accept-all) authentication

(def mail-config
 {:host "smtp-relay.sendinblue.com"
  :port 587
  :user "v1nc3ntpull1ng@gmail.com"
  :pass "CcEAZqTdVrDfRPzS"
  :tls true})

;;login links use :bcrypt
;; passwords use :argon2


(defn clear-cookie [response cookie-name]
  (assoc-in response [:headers "Set-Cookie"]
            (str cookie-name "=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT")))


(defn clear-cookie-handler "clear cookie handler"
  [next-handler]
  (-> (fn [ring-req]
        (let [res (next-handler ring-req)]
          (res/header res "Location" "/")
          (res/set-cookie res "remewserid" "guest" {:http-only true :max-age 1})
          (clear-cookie res "remewserid")))
      (cookies/wrap-cookies)
      ))

(defn sanitize [bcrypt-hash]
  (java.net.URLEncoder/encode bcrypt-hash "UTF-8"))

(defn unsanitize [url-safe-bcrypt-hash]
  (java.net.URLDecoder/decode url-safe-bcrypt-hash "UTF-8"))

(defn current-timestamp []
  (.getTime (java.util.Date.)))

(defn wrap-demo-authentication "Checks username/password and store the username and remewserid in sesh cookie."
  [next-handler]
  (-> (fn [ring-req]
        (let [res (next-handler ring-req)
              params (:params ring-req)
              eletteraddress (get params "email")
              epass (get params "password")
              raw epass
              remews-result (-> (xt/q (xt/db user/!xtdb) '{:find [?password]
                                                           :where [[?u :user/email email]
                                                                   [?u :user/password ?password]]
                                                           :in [email]}
                                      eletteraddress)
                                ffirst)
              hashed-pass remews-result]
          ;(println ring-req params eletteraddress epass hashed-pass)
          (if (and eletteraddress (verify-with :argon2 epass hashed-pass))
              (-> {:status 200 :body "<html><body style='background:url(\"img/moonroll.gif\") no-repeat center center fixed;background-color: #303030;'></body><meta http-equiv='refresh' content='2.7;url=https://remuse.co/'></html>"}
                  (res/header "Location" "/")
                  (res/set-cookie "remewserid" "RMWS|WSR|V000|0" {:http-only true :max-age (* 60 60 24 3)})
                  (res/set-cookie "useremail" eletteraddress {:http-only true :max-age (* 60 60 24 3)}))
            (do
              {:status 401 :body "<html>Le login did not pan out.  <a href='login.html'>Back to login</a></html>"}))
          ))
       (cookies/wrap-cookies)
       (wrap-params)
      ;(auth/wrap-basic-authentication authenticate)
    ))

(defn wrap-magic-link-auth "This produces magic links.  And mails them out."
  [next-handler]
  (-> (fn [ring-req]
        (let [res (next-handler ring-req)
              params (:params ring-req)
              eletteraddress (get params "email")
              ;;get the user number via xtdb query on eletteraddress
               
              
              remews-result (-> (xt/q (xt/db user/!xtdb) '{:find [?remewserid]
                                        :where [[?u :user/email email]
                                                [?u :user/remewserid ?remewserid]]
                                        :in [email]}
                                   eletteraddress)
                             ffirst)
              remewserid (or remews-result "|-1")
              user-count (Integer/parseInt (re-find #"[^|]+$" remewserid))

              
              in-ts (current-timestamp)
              raw (str user-count in-ts)
              hash (hash-with :bcrypt raw)
              sanitized (sanitize hash)
              domain "http://remuse.co"
              link (str domain "/authentim8?uc=" user-count "&timestamp=" in-ts "&code=" sanitized)
              
              ]
          (if (>= user-count 0) 
            (do
              (postal/send-message 
               mail-config 
               {:from "editor@remuse.co" 
                :to eletteraddress 
                :subject "Remuse.co Login Link" 
                :body (str "Hello!  Thank you for joining the Remuse arstist, musician, listener community.  We send out emails that have all the latest track and album additions covered, you can opt-out any time.  We will send you only transactional emails about your account to this address, like magic login links such as this one. " link " Have an amazing day.")})
              {:status 200 ;:body (str "the email is   " eletteraddress " and the hash is " (hash-with :argon2 eletteraddress) " and the link is " link "")
               :body (str "<html style='body{font-size:31px;}'>
               Music and technology have often intersected in innovative ways to create memorable experiences. At Remuse, we're pushing this tradition forward, connecting musicians and listeners in a more intimate, immersive way.<br/><br/>Like a perfectly composed symphony, our login process is designed to be seamless, harmonious, and user-friendly. Just as a musician's vibrations reach out to touch the listener's soul, we have dispatched a login link to your e-mail address. This link is your personal gateway into the beautiful world of Remuse.<br/><Br/>Follow the magic login link sent to " eletteraddress " to embark on a journey of musical exploration and deep connection. Just as the listeners open their hearts to the music, open your e-mail and find your unique link.<br/><br/>Once you have followed the link, you can set a password, adding an extra layer of security to your account. As a Remuse premium member, you'll have access to a universe of music, emotion, and connection - all at your fingertips.<br/><br/>Music is all about connection, and at Remuse, we're all about connecting you with the music you love. So tune in, log on, and let the vibrations guide you. Thanks for being a part of our symphony.</html>")})
            {:status 200 :body (str "<html>No such user found.  <a href='/create-account'>Create a Remuse.co account</a><html>")})))
          ;;check if the e-mail exists in the DB
          ;;generate the login-link
          ;;send an e-mail here
          ;;with the login-link.))
      (wrap-params)))



;;http://localhost:8080/authentim8?uc=4&timestamp=1689659541688&code=%242y%2414%24XjtTJyMpAVHcnRMqz17M8ORrtinn2gIgudX%2FAoCtAxuQmh27bALgm

(defn wrap-magic-link-verify "This verifies magic links for logging in"
  [next-handler]
  (-> (fn [ring-req]
        (let [params (:params ring-req)
              user-quant (get params "uc")
              timestamp (get params "timestamp")
              code (get params "code")
              in-ts (current-timestamp)
              raw (str user-quant timestamp)
              hash (hash-with :bcrypt raw)
              sanitized (sanitize hash)
              hashes-match? (verify-with :bcrypt raw (unsanitize code))
              ;;get useremail via user-count
              remews-result (-> (xt/q (xt/db user/!xtdb) '{:find [?email]
                                                           :where [[?u :user/email ?email]
                                                                   [?u :user/remewserid remewserid]]
                                                           :in [remewserid]}
                                      (str "RMWS|WSR|V000|" user-quant))
                                ffirst)
              eletteraddress remews-result]
          ;;(println "eletteraddress!" remews-result)
          (if hashes-match?
            (-> {:status 200 :body "<html><body style='background:url(\"img/moonroll.gif\") no-repeat center center fixed;background-color: #303030;'></body><meta http-equiv='refresh' content='2.7;url=https://remuse.co'></html>"}
                (res/header "Location" "/")
                (res/set-cookie "useremail" eletteraddress {:http-only true :max-age (* 60 60 24 3)})
                (res/set-cookie "remewserid" (str "RMWS|WSR|V000|" user-quant) {:http-only true :max-age (* 60 60 24 3)}))
            {:status 200 :body (str "<html>Magic link not valid, please <a href='/magiclink.html'>request a new one.</a></html>") ;(str " | " user-quant " | " timestamp " | " hash " || " sanitized " | " code " | " hashes-match? " | " raw " ||| " in-ts)
             })))
      (cookies/wrap-cookies)
      (wrap-params)))

(defn wrap-login-verify "This verifies user/pw combo for logging in."
  [next-handler]
  (-> (fn [ring-req]
        (let [params (:params ring-req)
              _ (println ring-req)
              username (get params "user")
              pass (get params "pass")
              nextapex-result (-> (xt/q (xt/db user/!xtdb) '{:find [password]
                                                           :where [[?u :user/username ?username]
                                                                   [?u :user/password password]]
                                                           :in [?username]}
                                      username)
                                ffirst)
              hashes-match? (if (not (nil? nextapex-result)) (verify-with :argon2 pass nextapex-result) false)]
          ;(println "pw1: " pass " | pw2: " nextapex-result)
          (if hashes-match?
            (do (println "the username is : " username)
              (-> {:status 200 :body username}
                (res/set-cookie "username" username {:http-only true :max-age (* 60 60 24 3)})
                (res/set-cookie "nextapexid" (str "NEXT|APEX|V000|" username) {:http-only true :max-age (* 60 60 24 3)})
                ))
            {:status 401 :body (str "so not the vibe") ;(str " | " user-quant " | " timestamp " | " hash " || " sanitized " | " code " | " hashes-match? " | " raw " ||| " in-ts)
             })))
      (cookies/wrap-cookies)
      (wrap-params)))

(defn wrap-demo-router "A basic path-based routing middleware"
  [next-handler]
  (fn [ring-req]
    (case (:uri ring-req)
      
      ;"/authoriginal" (let [response  ((wrap-demo-authentication next-handler) ring-req)]
      ;          (if (= 401 (:status response)) ; authenticated?
      ;            response                     ; send response to trigger auth prompt
      ;            (-> (res/status response 302) ; redirect
      ;              (res/header "Location" (get-in ring-req [:headers "referer"]))))) ; redirect to where the auth originated
      "/authentik9" (let [response  ((wrap-demo-authentication next-handler) ring-req)]
                  (res/header response "Location" "/"))
      "/magic-link" (let [response ((wrap-magic-link-auth next-handler) ring-req)] response)
      "/authentim8" (let [response ((wrap-magic-link-verify next-handler) ring-req)] response)

      "/nextapex-login" (let [response ((wrap-login-verify next-handler) ring-req)] 
           response)

      "/ringreq" (str ring-req)
      "/logout" (let [response ((clear-cookie-handler next-handler) ring-req)]
                  response)
      ;"/create-account" (let [response-msg "hit the serva"] (do (println ring-req) {:status 200 :body response-msg}))
      ;; For any other route, delegate to next middleware
      (next-handler ring-req))))

(defn template "Takes a `string` and a map of key-values `kvs`. Replace all instances of `$key$` by value in `string`"
  [string kvs]
  (reduce-kv (fn [r k v] (str/replace r (str "$" k "$") v)) string kvs))

(defn get-modules [manifest-path]
  (when-let [manifest (io/resource manifest-path)]
    (let [manifest-folder (when-let [folder-name (second (rseq (str/split manifest-path #"\/")))]
                            (str "/" folder-name "/"))]
      (->> (slurp manifest)
        (edn/read-string)
        (reduce (fn [r module] (assoc r (keyword "hyperfiddle.client.module" (name (:name module))) (str manifest-folder (:output-name module)))) {})))))

(defn wrap-index-page
  "Server the `index.html` file with injected javascript modules from `manifest.edn`. `manifest.edn` is generated by the client build and contains javascript modules information."
  [next-handler resources-path manifest-path]
  (fn [ring-req]
    (if-let [response (res/resource-response (str resources-path "/index.html"))]
      (if-let [modules (get-modules manifest-path)]
        (-> (res/response (template (slurp (:body response)) modules)) ; TODO cache in prod mode
          (res/content-type "text/html") ; ensure `index.html` is not cached
          (res/header "Cache-Control" "no-store")
          (res/header "Last-Modified" (get-in response [:headers "Last-Modified"])))        ;; No manifest found, can't inject js modules
        (-> (res/not-found "Missing client program manifest")
          (res/content-type "text/plain")))
      ;; index.html file not found on classpath
      (next-handler ring-req))))

(def ^:const VERSION (not-empty (System/getProperty "HYPERFIDDLE_ELECTRIC_SERVER_VERSION"))) ; see Dockerfile

(defn wrap-reject-stale-client
  "Intercept websocket UPGRADE request and check if client and server versions matches.
  An electric client is allowed to connect if its version matches the server's version, or if the server doesn't have a version set (dev mode).
  Otherwise, the client connection is rejected gracefully."
  [next-handler]
  (fn [ring-req]
    (let [client-version (get-in ring-req [:query-params "HYPERFIDDLE_ELECTRIC_CLIENT_VERSION"])]
      (cond
        (nil? VERSION)             (next-handler ring-req)
        (= client-version VERSION) (next-handler ring-req)
        :else (adapter/reject-websocket-handler 1008 "stale client") ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
        ))))

(def websocket-middleware
  (fn [next-handler]
    (-> (cookies/wrap-cookies next-handler) ; makes cookies available to Electric app
      (wrap-reject-stale-client)
      (wrap-params))))

(defn not-found-handler [_ring-request]
  (-> (res/not-found "Not found")
    (res/content-type "text/plain")))

(defn http-middleware [resources-path manifest-path]
  ;; these compose as functions, so are applied bottom up
  (-> not-found-handler
    (wrap-index-page resources-path manifest-path) ; 4. otherwise fallback to default page file
    (wrap-resource resources-path) ; 3. serve static file from classpath
    (wrap-content-type) ; 2. detect content (e.g. for index.html)
    (wrap-demo-router) ; 1. route
    (wrap-multipart-params)))

;:body #object[org.eclipse.jetty.server.HttpInputOverHTTP 0x1b681d28 HttpInputOverHTTP@1b681d28[c=0,q=0,[0]=null,s=STREAM]],


(defn- add-gzip-handler
  "Makes Jetty server compress responses. Optional but recommended."
  [server]
  (.setHandler server
    (doto (GzipHandler.)
      #_(.setIncludedMimeTypes (into-array ["text/css" "text/plain" "text/javascript" "application/javascript" "application/json" "image/svg+xml"])) ; only compress these
      (.setMinGzipSize 1024)
      (.setHandler (.getHandler server)))))

(defn start-server! [{:keys [port resources-path manifest-path]
                                   :or   {port            8080
                                          resources-path "public"
                                          manifest-path  "public/js/manifest.edn"}
                                   :as   config}]
  (try
    (let [server (ring/run-jetty (http-middleware resources-path manifest-path)
                   (merge {:port port
                           :join? false
                           :configurator add-gzip-handler
                           ;; Jetty 9 forces us to declare WS paths out of a ring handler.
                           ;; For Jetty 10 (NOT Java 8 compatible), drop the following and use `wrap-electric-websocket` as above
                           :websockets {"/" (websocket-middleware
                                              (fn [ring-req]
                                                (adapter/electric-ws-adapter
                                                  (partial adapter/electric-ws-message-handler
                                                    (auth/basic-authentication-request ring-req authenticate)))))}}
                     config))
          final-port (-> server (.getConnectors) first (.getPort))]
      (println "\n👉 App server available at" (str "http://" (:host config) ":" final-port "\n"))
      server)

    (catch IOException err
      (if (instance? BindException (ex-cause err))  ; port is already taken, retry with another one
        (do (log/warn "Port" port "was not available, retrying with" (inc port))
            (start-server! (update config :port inc)))
        (throw err)))))

