(ns kubiq)

;;namespace of aliasing purposes

(def force-toolkit-init
  (when-not *compile-files*
    (println "Initializing JavaFX")
    (javafx.embed.swing.JFXPanel.)))
