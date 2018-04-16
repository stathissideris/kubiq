(ns kubiq.web
  (:require [kubiq.fx :as fx]))

(defn execute! [w source]
  @(fx/run-later!
    #(-> w .getEngine (.executeScript source))))

(defn call [o method & args]
  (.call o method (into-array Object args)))

(defn append-child! [o child]
  (call o "appendChild" child))

(defn document [w] (execute! w "document"))

(defn element [w type]
  (call (document w) "createElement" type))

(defn text-node [w text]
  (call (document w) "createTextNode" text))

(defn member [o name]
  (.getMember o name))

(defn set-member! [o name value]
  (.setMember o name value))

(defn set-members! [o m]
  (doseq [[k v] m]
    (set-member! o k v)))

(defn body [w]
  (member (document w) "body"))
