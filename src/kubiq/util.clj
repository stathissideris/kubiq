(ns kubiq.util
  (:refer-clojure :exclude [meta alter-meta!])
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import java.util.WeakHashMap))

(defn camel->kebab [from]
  (let [s (str/split (name from) #"(?=[A-Z])" )]
    (apply str (interpose "-" (map str/lower-case s)))))

(defn kebab->camel [from]
  (let [s (str/split (name from) #"\-")]
    (apply str (first s) (map str/capitalize (next s)))))

(defn capitalize-first
  "Capitalizes first letter and leaves the rest of the string unchanged"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))

(defn resource->external-form [x]
  (if (str/starts-with? x "file:")
    x
    (.toExternalForm (io/resource x))))

;;;;;;;;;;;;;;;;;;;; meta ;;;;;;;;;;;;;;;;;;;;

(def ^:private meta-map (WeakHashMap.))
(defn meta [o]
  (.get meta-map o))

(defn add-meta! [o m]
  (.put meta-map o (merge (.get meta-map o) m)))

(defn alter-meta! [o fun & args]
  (locking meta-map
    (.put meta-map o (apply fun (or (meta o) {}) args))
    (meta o)))
