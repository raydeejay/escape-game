(ns escape-game.core.desktop-launcher
  (:require [escape-game.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. escape-game-game "escape-game" 800 480)
  (Keyboard/enableRepeatEvents true))
