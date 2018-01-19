(ns kubiq.reflection
  (:require [kubiq.util :as util])
  (:import [javafx.collections ObservableList]))

(defn superclasses [clazz]
  (when-let [super (.getSuperclass clazz)]
    (cons super (lazy-seq (superclasses super)))))

(defn methods [^Class class]
  (.getMethods class))

(defn getter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         util/capitalize-first
                         (str "get"))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn getter [clazz field-kw]
  (when-let [getter (getter-method clazz field-kw)]
    (fn [object] (.invoke getter object (object-array [])))))

(defn setter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         util/capitalize-first
                         (str "set"))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn setter [clazz field-kw]
  (if-let [setter (setter-method clazz field-kw)]
    (fn [object value]
      (.invoke setter object (object-array [value]))
      object)
    (when-let [getter-method (getter-method clazz field-kw)]
      (when (= ObservableList (.getReturnType getter-method)) ;;support for setting observable list fields
        (let [getter (getter clazz field-kw)]
          (fn [object value]
            (.setAll (getter object) (remove nil? value))
            object))))))
