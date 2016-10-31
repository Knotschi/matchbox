(ns matchbox.testing
  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go]]))
  (:require
    [matchbox.core :as m]
    [matchbox.async :as ma]
    [#?(:clj  clojure.core.async
        :cljs cljs.core.async) :refer [<! #?@(:clj [go <!!])]]))

;apiKey: "AIzaSyA6FhtI8zPp-HVw7x0wHfxGLKVK3Nl5kQo",
;authDomain: "matchbox-test.firebaseapp.com",
;databaseURL: "https://matchbox-test.firebaseio.com",
;storageBucket: "matchbox-test.appspot.com",

(def db-uri "https://matchbox-test.firebaseio.com")

(def pending (atom {}))
(def errors (atom {}))

(defn random-ref []
  (let [opts #?(:clj (m/init-server-options "matchbox-test" "test/matchbox/matchbox-tester-credentials.json")
                :cljs (m/init-web-options "AIzaSyA6FhtI8zPp-HVw7x0wHfxGLKVK3Nl5kQo" "matchbox-test"))
        _ (m/init opts)
        rf (m/connect db-uri (str (rand-int 100000)))]
    ;; clear data once connection closed, having trouble on JVM with reflection
    #?(:cljs (-> rf .onDisconnect .remove))
    rf))

#?(:clj
(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env))))

#?(:clj
(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else)))

(defmacro block-test
  "Ensuring blocking or continuation, run forms within a go block"
  [& body]
  `(let [complete# (~'chan)]
     (~'go (let [res# (or (try ~@body
                             (if-cljs
                               '(catch js/Object e# e#)
                               '(catch Exception e# e#)))
                        true)]
           (~'>! complete# res#)))
     (if-cljs
       '(async done (go (<! complete#) (done)))
       '(<!! complete#))))

(defmacro is=
  "Test next value delivered from channel matches expectation"
  [expect expr]
  `(block-test
    (~'is (= ~expect (~'<! ~expr)))))

(defmacro with<
  "Test next value delivered from channel matches expectation"
  [ref bind & body]
  `(block-test
     (let [~bind (~'<! (ma/deref< ~ref))]
       ~@body)))

(defmacro round-trip= [expectation data]
  `(block-test
     (let [ref# (random-ref)]
       (m/reset! ref# ~data)
       (let [result# (~'<! (ma/deref< ref#))]
         (~'is (= ~expectation result#))))))

(defmacro round-trip< [data bind & body]
  `(let [ref# (random-ref)]
     (m/reset! ref# ~data)
     (with< ref# ~bind ~@body)))

