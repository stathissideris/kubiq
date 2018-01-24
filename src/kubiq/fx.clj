(ns kubiq.fx
  (:refer-clojure :exclude [parents methods tree-seq])
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [kubiq :as k]
            [kubiq.util :as util]
            [kubiq.reflection :refer [superclasses methods getter-method getter setter-method setter]]
            [kubiq.traversal-impl :as traversal]
            [kubiq.text-impl :as text]
            [clojure.walk :as walk]
            [hawk.core :as hawk]
            [me.raynes.fs :as fs]
            [clojure.set :as set])
  (:import [javafx.collections ObservableList ListChangeListener]
           [javafx.scene.control SplitPane TableView ListView]
           [javafx.scene.text TextFlow]
           [javafx.scene.layout GridPane]
           [javafx.embed.swing JFXPanel]
           [javafx.application Platform]
           [javafx.stage StageStyle]
           [javafx.event EventHandler Event]
           [javafx.scene.paint Color]
           [javafx.beans.value ChangeListener]
           [javafx.scene Node]
           [javafx.scene.input Clipboard ClipboardContent]
           [javafx.concurrent Worker]
           [com.sun.javafx.stage StageHelper]
           [javafx.util Callback]
           [javafx.scene.web WebView]
           [java.net URI]
           [org.w3c.dom.events EventListener]
           [javax.swing ImageIcon]
           [java.util.concurrent CountDownLatch]
           [javax.swing SwingUtilities]))

;;;; utils ;;;;;

(defn init []
  (println "Initializing JavaFX...")
  (let [latch (CountDownLatch. 1)]
    (SwingUtilities/invokeLater
     #(do (javafx.embed.swing.JFXPanel.)
          (.countDown latch)))
    (.await latch))
  ;;(Platform/setImplicitExit false)
  )

(defn on-fx-thread? []
  (Platform/isFxApplicationThread))

(defn thread-probe [label]
  (when (on-fx-thread?) (prn '!!!ON-FX-THREAD label)))

(defn run-later! [fun]
  (let [p (promise)]
    (if (on-fx-thread?)
      (deliver p (fun))
      (Platform/runLater
       (fn []
         (try
           (deliver p (fun))
           (catch Exception e
             (.printStackTrace e)
             (deliver p e)
             (throw e))))))
    p))

(defn event-handler [fun]
  (reify EventHandler
    (^void handle [_ ^Event event]
     (fun event))))

(defn change-listener
  ([fun]
   (reify ChangeListener
     (changed [_ observable old new]
       (fun observable old new))))
  ([source fun]
   (reify ChangeListener
     (changed [_ observable old new]
       (fun source observable old new)))))

(defn one-off-change-listener
  ([fun]
   (reify ChangeListener
     (changed [this observable old new]
       (fun observable old new)
       (when new ;;remove only of the value changes to something non-nil and not false
         (.removeListener observable this))))))

(defn list-change-listener [fun]
  (reify ListChangeListener
    (onChanged [this change]
      (fun (seq (.getList change))))))

(defn callback [fun]
  (reify Callback
    (call [this p]
      (fun p))))

(defn parse-bbox [bbox]
  {::k/min-x  (.getMinX   bbox)
   ::k/max-x  (.getMaxX   bbox)
   ::k/min-z  (.getMinZ   bbox)
   ::k/width  (.getWidth  bbox)
   ::k/max-z  (.getMaxZ   bbox)
   ::k/depth  (.getDepth  bbox)
   ::k/max-y  (.getMaxY   bbox)
   ::k/min-y  (.getMinY   bbox)
   ::k/height (.getHeight bbox)})

(defn bounds-in-screen [component]
  (parse-bbox (.localToScreen component (.getBoundsInLocal component))))

(defn bounds-in-scene [component]
  (parse-bbox (.localToScene component (.getBoundsInLocal component))))

(defn bounds-in-parent [component]
  (parse-bbox (.getBoundsInParent component)))

;;;;;;;;;;;;;;;;;;;;
;;;;; mutation ;;;;;
;;;;;;;;;;;;;;;;;;;;

(defn lookup
  ([selector]
   (apply set/union (map #(lookup (-> % .getScene .getRoot) selector) (StageHelper/getStages))))
  ([component selector]
   (into #{} (-> component (.lookupAll selector) .toArray))))

(defn set-field!-dispatch
  [o field _] [(class o) field])
(defmulti set-field! set-field!-dispatch)

(defmethod set-field! [Object ::k/setup]
  [o _ value]
  (value o)
  o)

(defmethod set-field! [Object ::k/focused?]
  [o _ focus?]
  (when focus?
    (run-later! #(.requestFocus o))))

(defmethod set-field! [Object ::k/event-filter]
  [o _ [filter fun]]
  (.addEventFilter o filter (event-handler fun)))

(defn get-property [object field]
  (clojure.lang.Reflector/invokeInstanceMethod
   object
   (str (util/kebab->camel field) "Property")
   (object-array [])))

(defmethod set-field! [Object ::k/prop-listener]
  [o _ [prop fun]]
  (.addListener (get-property o prop) (change-listener o fun)))

(declare fset-in!)
(defn fset! [object field value]
  (when object
    (cond
      (and (= object ::k/top-level) (= field ::k/children))
      (StageHelper/getStages)

      (string? object)
      (let [objs (lookup object)]
        (fset! (first objs) field value))

      (integer? field) ;;ObservableList
      (.set object field value)

      (vector? field)
      (fset-in! object field value)

      (and (not= "kubiq.fx" (namespace field)) ;; :kubik.fx/ always skips set-field!
           (get-method set-field! (set-field!-dispatch object field value)))
      (set-field! object field value)

      :else
      (try
        (clojure.lang.Reflector/invokeInstanceMethod
         object
         (->> field
              util/kebab->camel
              util/capitalize-first
              (str "set"))
         (object-array [value]))
        object
        (catch Exception _
          (let [s! (setter (class object) field)]
            (if-not s!
              (throw (ex-info "setter not found"
                              {:object object
                               :class  (class object)
                               :field  field
                               :value  value}))
              (try
                (s! object value)
                object
                (catch Exception e
                  (throw (ex-info "error while calling setter"
                                  {:object object
                                   :class  (class object)
                                   :field  field
                                   :value  value}
                                  e))))))))))
  object)

;;;;;;;;;;;;
;; access ;;
;;;;;;;;;;;;
;;;OK TO HERE;;;
(defn get-field-dispatch [o field] [(class o) field])
(defmulti get-field get-field-dispatch)

(defmethod get-field [ListView ::k/visible-range] ;;;;;;;;;;;;;;;;;;
  [o _]
  (let [virtual-flow (some-> o .getSkin .getChildren (.get 0))]
    [(some-> virtual-flow .getFirstVisibleCell .getIndex)
     (some-> virtual-flow .getLastVisibleCell .getIndex)]))

(defmethod get-field [TableView ::k/visible-range]
  [o _]
  (let [virtual-flow (some-> o .getSkin .getChildren (.get 1))]
    [(some-> virtual-flow .getFirstVisibleCell .getIndex)
     (some-> virtual-flow .getLastVisibleCell .getIndex)]))

(defn fget [object field]
  (cond (and (= object ::k/top-level) (= field ::k/children))
        (StageHelper/getStages)

        (string? object)
        (let [objs (lookup object)]
          (fget (first objs) field))

        (integer? field) ;;ObservableList
        (.get object field)

        (and (not= "kubiq.fx" (namespace field)) ;; :kubik.fx/ always skips get-field
             (get-method get-field (get-field-dispatch object field)))
        (get-field object field)

        :else
        (clojure.lang.Reflector/invokeInstanceMethod
         object
         (->> field
              util/kebab->camel
              util/capitalize-first
              (str "get"))
         (object-array []))))

;;(s/def ::path (s/coll-of (s/or :field-name keyword? :index nat-int?)))
(defn fget-in [root path]
  (reduce (fn [o field]
            (try
              (fget o field)
              (catch Exception e
                (throw (ex-info "fget-in failed"
                                {:path           path
                                 :root           root
                                 :current-object o
                                 :current-field  field}
                                e))))) root path))
(s/fdef fget-in
  :args (s/cat :root some? :path ::k/path))

(defn fset-in! [root path value]
  (let [field       (last path)
        parent-path (butlast path)]
    (try
      (fset! (fget-in root parent-path) field value)
      (catch Exception e
        (throw (ex-info "fset-in! failed"
                        {:path  path
                         :value value
                         :root  root}
                        e))))))
;; (s/fdef set-field-in!
;;   :args (s/cat :root some? :path ::path :value any?))

(defn update! [object field fun & args]
  (let [old-value (fget object field)]
    (fset! object field (apply fun old-value args))))

(defn set-fields! [object pairs]
  (doseq [[field value] pairs]
    (fset! object field value))
  object)

(defn insert-in! [root path value]
  (if (and (= root ::k/top-level) (= 2 (count path)))
    (do
      (.add (StageHelper/getStages) (last path) value)
      (.show value))
    (let [index       (last path)
          parent-path (butlast path)
          coll        (fget-in root parent-path)]
      (.add coll index value))))

(defn remove-in! [root path]
  (if (and (= root ::k/top-level) (= 2 (count path)))
    (let [stages (StageHelper/getStages)
          value  (.get stages (last path))]
      (.remove stages value)
      (.close value))
    (let [index       (last path)
          parent-path (butlast path)
          coll        (fget-in root parent-path)
          item        (.get coll index)]
      (.remove coll item)))) ;;removing by index does not work

(defn- resolve-class [class-kw]
  (if (keyword? class-kw)
    (Class/forName (str "javafx."
                        (namespace class-kw)
                        "."
                        (-> class-kw name util/kebab->camel util/capitalize-first)))
    class-kw))

(defn new-instance
  ([class-kw]
   (new-instance class-kw nil))
  ([class-kw args]
   (try
     (let [clazz (if (keyword? class-kw)
                   (resolve-class class-kw)
                   class-kw)]
       (if (empty? args)
         (.newInstance clazz)
         (clojure.lang.Reflector/invokeConstructor clazz (to-array args))))
     (catch Exception e
       (throw (ex-info "Error while creating FX instance"
                       {:class class-kw
                        :args  args}
                       e))))))

;;;;;;;;;;;;;;;;
;;;;; make ;;;;;
;;;;;;;;;;;;;;;;

(defn- make-args [spec]
  (vec (second (first (filter #(= (first %) ::k/args) spec)))))

(defn- make-other [spec]
  (remove #(= (first %) ::k/args) spec))

(defn make-component
  ([class-or-instance]
   (make-component class-or-instance {}))
  ([class-or-instance spec]
   (try
    (cond (= ::k/top-level class-or-instance)
          ::k/top-level

          (= ::k/unmanaged class-or-instance)
          (::k/component spec)

          :else
          (let [o (if (or (keyword? class-or-instance)
                          (class? class-or-instance))
                    (new-instance class-or-instance (make-args spec))
                    class-or-instance)]
            (doseq [[field value :as entry] (make-other spec)]
              (when entry (fset! o field value)))
            o))
    (catch Exception e
      (throw (ex-info "Error while constructing component"
                      {:spec    spec
                       :message (.getMessage e)}
                      e))))))

(defn make
  [tree]
  (walk/postwalk
   (fn [item]
     (if (::k/type item)
       (make-component (::k/type item)
                       (dissoc item ::k/type))
       item))
   tree))

(defn unmanaged [component]
  {::k/type      ::k/unmanaged
   ::k/component component})

;;;;;;;;;
;; CSS ;;
;;;;;;;;;

(defn- reload-stylesheet! [component path]
  (doto component
    (-> .getStylesheets (.remove path))
    (-> .getStylesheets (.add path))))

(defn- watch-sheet! [component path]
  (let [wp (some-> path URI. fs/file .getPath)]
    {:path         path
     :watcher-path wp
     :file-watcher
     (hawk/watch! [{:paths   [wp]
                    :handler (fn [_ _]
                               (run-later!
                                #(reload-stylesheet! component path)))}])}))

(defmethod set-field! [Object ::k/stylesheets]
  [o _ paths]
  (let [paths (map util/resource->external-form paths)]
    (doto o
      (-> .getStylesheets .clear)
      (-> .getStylesheets (.addAll paths))
      (util/alter-meta! assoc ::k/stylesheets (mapv #(watch-sheet! o %) paths)))))

(defmethod set-field! [WebView ::k/stylesheet]
  [o _ path]
  (let [path (util/resource->external-form path)
        wp   (some-> path URI. fs/file .getPath)]
    (-> o .getEngine (.setUserStyleSheetLocation path))
    (util/alter-meta!
     o assoc ::k/stylesheet
     {:path         path
      :watcher-path wp
      :watcher      (hawk/watch! [{:paths   [wp]
                                   :handler (fn [_ _]
                                              (run-later!
                                               #(doto o
                                                  (-> .getEngine (.setUserStyleSheetLocation nil))
                                                  (-> .getEngine (.setUserStyleSheetLocation path)))))}])})))

;;;;;;;;;;;;;;;;;;
;;;;; Layout ;;;;;
;;;;;;;;;;;;;;;;;;

(defmethod set-field! [GridPane ::k/children]
  ([gp _ rows]
   (doall
    (map-indexed
     (fn [row-idx row]
       (doall
        (map-indexed
         (fn [col-idx item]
           (.add gp item col-idx row-idx))
         row)))
     rows))))

;;;;;;;;;;;;;;;;;;;
;;;;; "React" ;;;;;
;;;;;;;;;;;;;;;;;;;

(defn- type-change? [diff-group]
  (some?
   (first
    (filter #(and (= :edit (:type %))
                  (= :map (:struct %))
                  (= ::k/type (last (:path %)))) diff-group))))

(defn- ignore-diff? [{:keys [type path] :as diff}]
  (or (and (= :edit type) (contains? (set path) ::k/setup))
      (and (= :edit type) (contains? (set path) ::k/args))))

;;(require '[clojure.pprint :refer [pprint]])
(defn update-tree!
  [root diffs]
  ;;(pprint diffs)
  (let [diff-groups (partition-by #(vector (butlast (:path %)) (:struct %)) diffs)]
    (doseq [diff-group diff-groups]
      (cond
        (type-change? diff-group)
        (run-later!
         #(fset-in! root
                         (-> diff-group first :path butlast)
                         (make-component (->> diff-group
                                              (filter (comp #{:edit :assoc} :type))
                                              (map (juxt (comp last :path) :value))
                                              (into {})))))
        :else
        (doseq [{:keys [type path value] :as diff} (remove ignore-diff? diff-group)]
          (run-later!
           #(condp = type
              :edit   (fset-in! root path (make value))
              :assoc  (fset-in! root path (make value))
              :dissoc (fset-in! root path nil)
              :insert (insert-in! root path (make value))
              :delete (remove-in! root path))))))))

;;;;;;;;;;;;;;;;;;;;;
;;;;; traversal ;;;;;
;;;;;;;;;;;;;;;;;;;;;

(def top-level (kubiq.traversal_impl.TopLevel.))

(defn- safe-id [component]
  (try (.getId component) (catch Exception _ nil)))

(defn tree-seq [root]
  (clojure.core/tree-seq traversal/children? traversal/children root))

(defn find-by-id
  ([id]
   (find-by-id top-level id))
  ([root id]
   (->> (tree-seq root)
        (filter #(= id (safe-id %)))
        first)))

(defn- safe-style-class [component]
  (try (set (.getStyleClass component))
       (catch Exception _ #{})))

(defn find-by-style-class [root clazz]
  (->> (tree-seq root)
       (filter #(get (safe-style-class %) clazz))))

(defn tree
  ([]
   (tree top-level))
  ([root]
   (when root
     (merge
      {:component root}
      (when-let [m (not-empty (util/meta root))]
        {:meta m})
      (when (traversal/children? root)
        {:children (mapv tree (traversal/children root))})))))

(defn stage-of [component]
  (some-> component .getScene .getWindow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; convenience functions ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn label
  [text & [spec]]
  (make
   (merge {::k/type :scene.control/label
           ::k/text (str text)}
          spec)))

(defn window [title root]
  (make {::k/type  :stage/stage
         ::k/scene {::k/type :scene/scene
                    ::k/args [root]}}))

(defn transparent-window [root]
  (make {::k/type  :stage/stage
         ::k/args  [StageStyle/TRANSPARENT]
         ::k/scene {::k/type :scene/scene
                    ::k/args [root]
                    ::k/fill   Color/TRANSPARENT}}))

(defn undecorated-window [root]
  (make {::k/type  :stage/stage
         ::k/args  [StageStyle/UNDECORATED]
         ::k/scene {::k/type :scene/scene
                    ::k/args [root]}}))

(defn show! [c] (.show c) c)

(defn has-style-class? [node ^String c]
  (and (instance? javafx.scene.Node node)
       (some? (not-empty (filter (partial = c) (seq (.getStyleClass node)))))))

(defn parent [^Node node]
  (when node (.getParent node)))

(defn parents [^Node node]
  (take-while (complement nil?) (rest (iterate parent node))))

(defn focus-owner [stage]
  (some-> stage .getScene .focusOwnerProperty .get))

;;;;;;;;;;;;;;;;
;;;;; text ;;;;;
;;;;;;;;;;;;;;;;

(extend-type String
  text/Text
  (text [this] (make {::k/type :scene.text/text
                      ::k/args [this]})))

(defmethod set-field! [TextFlow ::k/children]
  [tf _ nodes]
  (-> tf .getChildren (.setAll (mapv text/text (remove nil? nodes)))))

(def font text/font)
(def span text/span)
(def text-flow text/text-flow)

;;;;;;;;;;;;;;;;;;;; color ;;;;;;;;;;;;;;;;;;;;

(defn color
  ([web]
   (Color/web web))
  ([r g b]
   (Color/color r g b))
  ([r g b a]
   (Color/color r g b a)))

(defn darker [color] (.darker color))
(defn brighter [color] (.brighter color))
(defn desaturated [color] (.desaturate color))
(defn saturated [color] (.saturate color))
(defn grayscale [color] (.grayscale color))
(defn inverted [color] (.invert color))

;;;;;;;;;;;;;;;;;;;; clipboard ;;;;;;;;;;;;;;;;;;;;

(defn to-clipboard [text]
  (doto (Clipboard/getSystemClipboard)
    (.setContent (doto (ClipboardContent.)
                   (.putString text)))))

;;;;;;;;;;;;;;;;;;;; web ;;;;;;;;;;;;;;;;;;;;

(defn dom-event-listener [fun]
  (reify EventListener
    (^void handleEvent [_ ^org.w3c.dom.events.Event event]
     (fun event))))

;;from: http://blogs.kiyut.com/tonny/2013/07/30/javafx-webview-addhyperlinklistener/
(defmethod set-field! [WebView ::k/link-listener]
  [this _ listener]
  (when listener
    (-> this .getEngine .getLoadWorker .stateProperty
        (.addListener
         (change-listener
          (fn [_ old new]
            (when (= new javafx.concurrent.Worker$State/SUCCEEDED)
              (let [event-listener (dom-event-listener
                                    (fn [event]
                                      (when-let [href (some-> event .getTarget (.getAttribute "href"))]
                                        (listener event href (keyword (.getType event))))))
                    nodes (-> this .getEngine .getDocument (.getElementsByTagName "a"))]
                (doseq [idx (range (.getLength nodes))]
                  (.addEventListener (.item nodes idx) "click" event-listener false)
                  (.addEventListener (.item nodes idx) "mouseover" event-listener false))))))))))

;;see com.sun.deploy.uitoolkit.impl.fx.HostServicesFactory/showDocument
(defn open-in-browser [url]
  (let [os (System/getProperty "os.name")]
    (cond
      (str/starts-with? os "Mac OS")
      (eval `(com.apple.eio.FileManager/openURL ~url))
      (str/starts-with? os "Windows")
      (-> (Runtime/getRuntime) (.exec (str "rundll32 url.dll,FileProtocolHandler " url)))
      :else
      (let [browsers ["google-chrome" "firefox" "opera" "konqueror" "mozilla"]]
        ;;TODO linux: use Runtime.getRuntime().exec() to find which browser is installed
        ;;and use the same method to invoke the browser with the passed URL
        ))))

;;;;;;;;;;;;;;;;;;;; text-areas ;;;;;;;;;;;;;;;;;;;;

(defn insert-text! [text-field text]
  (let [pos      (fget text-field :caret-position)
        old-text (fget text-field :text)]
    (fset! text-field
           :text (str (subs old-text 0 pos)
                      text
                      (subs old-text pos)))
    (.positionCaret text-field (+ pos (count text)))))

(defn caret-left
  ([text-field]
   (caret-left text-field 1))
  ([text-field steps]
   (.positionCaret text-field (max 0 (- (.getCaretPosition text-field) steps)))))

(defn caret-right
  ([text-field]
   (caret-right text-field 1))
  ([text-field steps]
   (.positionCaret text-field (+ (.getCaretPosition text-field) steps))))

;;;;;;;;;;;;;;;;;;;; application ;;;;;;;;;;;;;;;;;;;;

;;TODO this is interesting as well:
;; (-> (com.apple.eawt.Application/getApplication)
;;     (.setDockIconBadge "2"))

(defn set-app-icon [resource-path]
  (let [os (System/getProperty "os.name")]
    (cond
      (str/starts-with? os "Mac OS")
      (eval
       `(let [icon-url# (.getResource Class ~resource-path)
              image#    (.getImage (ImageIcon. icon-url#))]
          (-> (com.apple.eawt.Application/getApplication)
              (.setDockIconImage image#))))
      (str/starts-with? os "Windows")
      ;;TODO
      :else
      ;;TODO linux
      )))

(defn add-to-fake-scene
  "Sometimes you need to add a component in a fake scene in order to
  measure it before placing it in the actual scene where it needs to
  be. This function does that, then calls .applyCss and .layout on the
  passed component to force layout and returns the component for
  further processing.

  See:

  https://stackoverflow.com/questions/26152642/get-the-height-of-a-node-in-javafx-generate-a-layout-pass

  and

  https://stackoverflow.com/questions/13015698/how-to-calculate-the-pixel-width-of-a-string-in-javafx"
  [component]
  (make
   {::k/type :scene/scene
    ::k/args [component]})
  (.applyCss component)
  (.layout component)
  component)


(comment
  (make-component
   :scene.control/button
   [[::k/args ["foo"]]
    [::k/text "bar"]
    [::k/setup #(.setText % "baz")]]))

(comment
  (make
   {::k/type        :scene.control/split-pane
    ::k/orientation javafx.geometry.Orientation/HORIZONTAL
    ::k/items       [{::k/type  :scene.control/label
                      ::k/text "foo"}
                     {::k/type  :scene.control/label
                      ::k/text "bar"}
                     {::k/type :scene.layout/border-pane
                      ::k/center {::k/type  :scene.control/label
                                  ::k/text "baz"}
                      ::k/bottom {::k/type  :scene.control/label
                                  ::k/text "zoo"}}]}))

(comment
  (def s (-> (new-instance :scene.control/split-pane)
             (set-field! :items [(new-instance :scene.control/button ["1"])
                                 (new-instance :scene.control/button ["2"])
                                 (new-instance :scene.control/button ["3"])])))
  (fget s ::k/items))
