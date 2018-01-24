(ns kubiq.text-impl
  (:require [kubiq :as k])
  (:import [javafx.scene.text Font FontWeight FontPosture TextAlignment TextFlow]
           [javafx.scene Node]))

(defprotocol Text
  (text [this]))

(extend-type javafx.scene.text.Text
  Text
  (text [this] this))

(defn font-defaults []
  (let [default-font (Font/getDefault)]
    {::family  (.getFamily default-font)
     ::weight  FontWeight/NORMAL
     ::posture FontPosture/REGULAR
     ::size    (.getSize default-font)}))

(def font-weight-map
  {"black"       FontWeight/BLACK
   "bold"        FontWeight/BOLD
   "extra-bold"  FontWeight/EXTRA_BOLD
   "extra-light" FontWeight/EXTRA_LIGHT
   "light"       FontWeight/LIGHT
   "medium"      FontWeight/MEDIUM
   "normal"      FontWeight/NORMAL
   "semi-bold"   FontWeight/SEMI_BOLD
   "thin"        FontWeight/THIN})

(def font-posture-map
  {"italic"  FontPosture/ITALIC
   "regular" FontPosture/REGULAR})

(defn font [{:keys [family weight posture size] :as options}]
  (let [options (cond-> options
                  weight  (assoc ::weight (font-weight-map weight))
                  posture (assoc ::posture (font-posture-map posture)))
        {:keys [family weight posture size]}
        (merge (font-defaults) options)]
    (Font/font family weight posture size)))

(def text-alignment-map
  {:center  TextAlignment/CENTER
   :justify TextAlignment/JUSTIFY
   :left    TextAlignment/LEFT
   :right   TextAlignment/RIGHT})

(defn span [{:keys [underline strike align fill] :as attr} content]
  (let [font-attr (not-empty (select-keys attr [:family :weight :posture :size]))
        f         (when font-attr (font font-attr))
        text
        (doto (javafx.scene.text.Text. content)
          (.setUnderline (or underline false))
          (.setStrikethrough (or strike false))
          (.setTextAlignment (text-alignment-map (or align :left))))]
    (when f (.setFont text f))
    (when fill
      (.setFill text fill))
    text))

(extend-type clojure.lang.APersistentVector
  Text
  (text [this]
    (let [tag     (first this)
          attr    (when (map? (second this)) (second this))
          content (if attr (drop 2 this) (rest this))]
      (when (not (every? (some-fn nil? string?) content))
        (throw (ex-info "text hiccup tags cannot be nested" {:tag this})))
      (let [content (apply str (remove nil? content))]
        (condp = tag
          :span (span attr content)
          :b    (span {::weight "bold"} content)
          :i    (span {::posture "italic"} content)
          :u    (span {::underline true} content)
          :del  (span {::strike true} content))))))

(defn text-flow
  ([]
   (TextFlow.))
  ([nodes]
   (if (empty? nodes)
     (TextFlow.)
     (TextFlow. (into-array Node (map text (remove nil? nodes)))))))
