(ns kubiq)

;;namespace of aliasing purposes

(def force-toolkit-init-for-compilation
  (when *compile-files*
    (println "Initializing JavaFX for compilation...")
    (javafx.embed.swing.JFXPanel.)))

(def force-toolkit-init
  (when-not *compile-files*
    (println "Initializing JavaFX...")
    (javafx.embed.swing.JFXPanel.)))
