(ns escape-game.core
  (:require [escape-game.utils :refer :all]
            [escape-game.actions :refer :all]
            [escape-game.gamedata :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d :refer :all] ))


;; forward declarations
(declare escape-game-game main-screen win-screen)


;; asset manager
(defonce manager (asset-manager))


;; main screen - where the game is played
(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (let [newroom (room->entities :room01 rooms)]
      (update! screen
               :current-room :room01
               :rooms {:room01 newroom}
               :inventory []
               :selected nil)
      newroom))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen [(assoc (texture "images/inventory.png")
                            :x 700 :y 0
                            :width 100 :height 480)])
    (render! screen (remove #(:hidden %) entities))
    (render! screen (:inventory screen))
    (if (:selected screen)
      (render! screen [(assoc (texture "images/inventory-selected.png")
                              :x (:x (:selected screen))
                              :y (- (:y (:selected screen)) 3)
                              :width 47 :height 58)]))
    entities)       ; WATCH: must return only the entities list! (or NIL?)

  :on-resize
  (fn [screen entities]
    (height! screen 480))

  :on-key-down
  (fn [screen entities]
    (cond (= (:key screen) (key-code :n))
          (do (set-screen! escape-game-game main-screen)
              nil)
          (= (:key screen) (key-code :c))
          (do (println (count entities) (count (:inventory screen)))
              nil)
          (= (:key screen) (key-code :r))
          (do (room->entities (:current-room screen) rooms))
          ;; (= (:key screen) (key-code :s))
          ;; (do (input! :get-text-input #(println %) "Spawn item" "text" "hint")
          ;;     nil)
          (= (:key screen) (key-code :q))
          (do (app! :exit)
              nil)
          :else
          nil))

  :on-touch-down
  (fn [screen entities]
    "I check for clicks on entities, first in the inventory and then
    in the entities vector. I should probably be smart and consider
    how objects cover other objects."
    (mapv
     (fn [ent]
       (cond (and (clicked? ent screen)
                  (:action ent))
             (do (debug "ACTIVATING INVENTORY ACTION for" (:name ent))
                 ((:action ent) ent screen entities))
             :else
             ent))
     (:inventory screen))
    (mapv
     (fn [ent]
       (cond (and (not (:hidden ent)) (clicked? ent screen)
                  (:action ent))
             (do (debug "ACTIVATING ACTION for" (:name ent))
                 ((:action ent) ent screen entities))
             :else
             ent))
     entities))

  :switch-to-room
  (fn [screen entities]
    (let [from (screen :current-room)
          target (screen :target)
          newroom (or ((screen :rooms) target)
                        (room->entities target rooms))]
      (update! screen
               :rooms (merge (screen :rooms)
                             {from entities}
                             {target newroom})
               :current-room target)
      newroom)))


;; screen that you get when you win
(defscreen win-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    nil)

  :on-render
  (fn [screen entities]
    (clear!)
    entities)

  :on-resize
  (fn [screen entities]
    (height! screen 480))

  :on-key-down
  (fn [screen entities]
    (cond (= (:key screen) (key-code :n))
          (do (set-screen! escape-game-game main-screen)
              nil)
          (= (:key screen) (key-code :c))
          (do (println (count entities) (count (:inventory screen)))
              nil)
          (= (:key screen) (key-code :q))
          (do (app! :exit)
              nil)
          :else
          nil))

  :on-touch-down
  (fn [screen entities]
    (app! :exit)))


;; disaster prevention
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


;; entry point for the game
(defgame escape-game-game
  :on-create
  (fn [this]
    (set-screen-wrapper! (fn [screen screen-fn]
                           (try (screen-fn)
                                (catch Exception e
                                  (.printStackTrace e)
                                  (set-screen! escape-game-game blank-screen)))))
    (set-asset-manager! manager)
    (set-screen! this main-screen)))
