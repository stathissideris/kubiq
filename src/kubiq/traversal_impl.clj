(ns kubiq.traversal-impl
  (:import [com.sun.javafx.stage StageHelper]
           [javafx.stage Stage]
           [javafx.scene Scene Group]
           [javafx.scene.layout Pane GridPane]
           [javafx.scene.control SplitPane ScrollPane TabPane Tab]))

(defprotocol Parent
  (children? [this])
  (children [this])
  (child [this index]))

(deftype TopLevel []
  Parent
  (children [this]
    (distinct (seq (StageHelper/getStages))))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
    (.get (children this) index)))

(extend-type Stage
  Parent
  (children [this] [(.getScene this)])
  (children? [this] true)
  (child [this index] (.getScene this)))

(extend-type Scene
  Parent
  (children [this] [(.getRoot this)])
  (children? [this] true)
  (child [this index] (.getRoot this)))

(extend-type Pane
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type SplitPane
  Parent
  (children [this]
    (.getItems this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
    (.get (children this) index)))

(extend-type ScrollPane
  Parent
  (children [this]
    [(.getContent this)])
  (children? [this]
    true)
  (child [this index]
    (.getContent this)))

(extend-type Group
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type GridPane
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type TabPane
  Parent
  (children [this]
    (.getTabs this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type Tab
  Parent
  (children [this]
    [(.getContent this)])
  (children? [this]
    1)
  (child [this index]
    (first (children this))))

(extend-type Object
  Parent
  (children [this] nil)
  (children? [this] false)
  (child [this index] nil))
