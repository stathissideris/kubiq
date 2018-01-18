(ns kubiq.observable
  (:import [javafx.collections FXCollections]))

(defmulti observable-list type)

(defmethod observable-list clojure.lang.APersistentVector
  [^java.util.List x]
  (FXCollections/observableArrayList x))

(defmethod observable-list clojure.lang.LazySeq
  [^java.util.List x]
  (FXCollections/observableArrayList x))

(defmethod observable-list clojure.lang.ASeq
  [^java.util.List x]
  (FXCollections/observableArrayList x))
