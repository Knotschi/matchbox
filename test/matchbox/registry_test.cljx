(ns matchbox.registry-test
  #+cljs (:require-macros [cemerick.cljs.test :refer [is deftest done]])
  (:require #+cljs [cemerick.cljs.test :as t]
            #+clj [clojure.test :as t :refer [deftest is]]
            [matchbox.registry :as r]))

(reset! r/unsubs {})

(def a-1 (atom nil))
(def a-2 (atom nil))
(def a-3 (atom nil))
(def a-4 (atom nil))

(def callback-1 #(swap! a-1 inc))
(def callback-2 #(swap! a-2 inc))
(def callback-3 #(swap! a-3 inc))
(def callback-4 #(swap! a-4 inc))

(deftest lifecycle-test
  (r/disable-listeners!)
  (is (empty? @r/unsubs))

  (reset! a-1 0)
  (reset! a-2 0)
  (reset! a-3 0)
  (reset! a-4 0)

  (r/register-listener "ref-like" :value callback-1)
  (r/register-listener "ref-like" :value callback-2)
  (r/register-listener "ref-like" :child-added callback-4)
  (r/register-listener "ref-ness" :child-removed callback-3)

  (is (= {"ref-like" {:value #{callback-1 callback-2}
                      :child-added #{callback-4}}
          "ref-ness" {:child-removed #{callback-3}}}
         @r/unsubs))

  (r/disable-listener! callback-1)
  (is (= {"ref-like" {:value #{callback-2}
                      :child-added #{callback-4}}
          "ref-ness" {:child-removed #{callback-3}}}
         @r/unsubs))

  (r/disable-listeners! "ref-like" :child-added)
  (is (= {"ref-like" {:value #{callback-2}}
          "ref-ness" {:child-removed #{callback-3}}}
         @r/unsubs))

  (r/disable-listeners! "ref-ness")
  (is (= {"ref-like" {:value #{callback-2}}}
         @r/unsubs))

  (r/disable-listeners!)
  (is (empty? @r/unsubs))

  (is (= 1 @a-1))
  (is (= 1 @a-2))
  (is (= 1 @a-3))
  (is (= 1 @a-4)))
