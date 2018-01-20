(ns kubiq.fx-test
  (:refer-clojure :exclude [parents methods tree-seq])
  (:require [kubiq :as k]
            [kubiq.fx :refer :all :as fx]
            [clojure.test :refer :all]))

(init)

(deftest simple-make
  (is (= "foo" (fget (make {::k/type :scene.control/label
                            ::fx/text "foo"})
                     ::fx/text))))

(deftest parents-test
  (let [foo (label "foo")
        c   (make-component
             :scene.layout/border-pane
             {::fx/id     "a"
              ::fx/center (make-component
                           :scene.layout/border-pane
                           {::fx/id     "b"
                            ::fx/center (make-component
                                         :scene.layout/border-pane
                                         {::fx/id     "c"
                                          ::fx/center (make-component
                                                       :scene.layout/border-pane
                                                       {::fx/id     "d"
                                                        ::fx/center foo})})})})]
    (is (= ["d" "c" "b" "a"] (map #(.getId %) (parents foo))))))

(deftest get-field-in-test
  (testing "simple"
    (let [foo (label "foo")
          c   (make-component
               :scene.layout/border-pane
               {::fx/id     "a"
                ::fx/center (make-component
                             :scene.layout/border-pane
                             {::fx/id     "b"
                              ::fx/center (make-component
                                           :scene.layout/border-pane
                                           {::fx/id     "c"
                                            ::fx/center (make-component
                                                         :scene.layout/border-pane
                                                         {::fx/id     "d"
                                                          ::fx/center foo})})})})]
      (is (= "foo" (fget-in c [::fx/center ::fx/center ::fx/center ::fx/center ::fx/text])))))
  (testing "with indexes"
    (let [foo (label "foo")
          c   (make-component
               :scene.control/split-pane
               {::fx/items
                [(make-component
                  :scene.control/split-pane
                  {::fx/items
                   [(make-component
                     :scene.control/split-pane
                     {::fx/items
                      [(make-component
                        :scene.control/split-pane
                        {::fx/items
                         [foo]})]})]})]})]
      (is (= "foo" (fget-in c [::fx/items 0
                               ::fx/items 0
                               ::fx/items 0
                               ::fx/items 0
                               ::fx/text]))))))

(deftest set-field-in!-test
  (let [foo (label "foo")
        c   (make-component
             :scene.layout/border-pane
             {::fx/id     "a"
              ::fx/center (make-component
                           :scene.layout/border-pane
                           {::fx/id     "b"
                            ::fx/center (make-component
                                         :scene.layout/border-pane
                                         {::fx/id     "c"
                                          ::fx/center (make-component
                                                       :scene.layout/border-pane
                                                       {::fx/id     "d"
                                                        ::fx/center foo})})})})]
    (fset-in! c [::fx/center ::fx/center ::fx/center ::fx/center ::fx/text] "bar")
    (is (= "bar" (fget-in c [::fx/center ::fx/center ::fx/center ::fx/center ::fx/text])))))

(deftest has-style-class?-test
  (let [c (make
           {::k/type         :scene.control/label
            ::fx/style-class ["foo" "bar"]
            ::fx/text        "test"})]
    (is (true? (has-style-class? c "foo")))
    (is (true? (has-style-class? c "bar")))
    (is (false? (has-style-class? c "baz")))))

(deftest set-fields-test
  (testing "with maps"
    (let [l (label "foo")]
      (set-fields! l {::fx/id      "the-id"
                      ::fx/style-class ["focusable"]})
      (is (= "the-id" (.getId l)))
      (is (= ["focusable"] (seq (.getStyleClass l))))))
  (testing "with vectors of vectors"
    (let [l (label "foo")]
      (set-fields! l [[::fx/id "the-id"]
                      [::fx/style-class ["focusable"]]])
      (is (= "the-id" (.getId l)))
      (is (= ["focusable"] (seq (.getStyleClass l)))))))

(deftest one-off-change-listener-test
  (let [log    (atom [])
        button (make-component :scene.control/button {::fx/text "foo"})]
    (-> button .textProperty (.addListener (one-off-change-listener (fn [_ _ _] (swap! log conj "called")))))
    (is (= [] @log))

    (.setText button "bar")
    (is (= ["called"] @log))

    (.setText button "baz")
    (is (= ["called"] @log))

    (.setText button "foobar")
    (is (= ["called"] @log))

    (.setText button "barbar")
    (is (= ["called"] @log))

    (.setText button "bazbar")
    (is (= ["called"] @log))))
