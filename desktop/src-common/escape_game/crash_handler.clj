(ns escape-game.crash-handler
  (:require [play-clj.core :refer :all]))

;; disaster prevention ?
(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!))

  :on-key-down
  (fn [screen entities]
    (when (= (:key screen) (key-code :n))
      (set-screen! escape-game-game main-screen))
    (when (= (:key screen) (key-code :q))
      (app! :exit))
    nil))
