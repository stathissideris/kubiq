(ns kubiq.traversal-impl
  (:import [com.sun.javafx.stage StageHelper]))

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

(extend-type javafx.stage.Stage
  Parent
  (children [this] [(.getScene this)])
  (children? [this] true)
  (child [this index] (.getScene this)))

(extend-type javafx.scene.Scene
  Parent
  (children [this] [(.getRoot this)])
  (children? [this] true)
  (child [this index] (.getRoot this)))

(extend-type javafx.scene.layout.Pane
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type javafx.scene.control.SplitPane
  Parent
  (children [this]
    (.getItems this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
    (.get (children this) index)))

(extend-type javafx.scene.control.ScrollPane
  Parent
  (children [this]
    [(.getContent this)])
  (children? [this]
    true)
  (child [this index]
    (.getContent this)))

(extend-type javafx.scene.Group
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type javafx.scene.layout.GridPane
  Parent
  (children [this]
    (.getChildren this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type javafx.scene.control.TabPane
  Parent
  (children [this]
    (.getTabs this))
  (children? [this]
    (< 0 (count (children this))))
  (child [this index]
   (.get (children this) index)))

(extend-type javafx.scene.control.Tab
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
