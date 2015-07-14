(ns escape-game.core
  (:require [escape-game.utils :refer :all]
            [escape-game.actions :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d :refer :all] ))


;; forward declarations
(declare escape-game-game main-screen win-screen rooms)


;; asset manager
(defonce manager (asset-manager))


;; some generic functions
(def debug? true)
(defn debug
  "I print debugging information if I should."
  [& r]
  (when debug? (apply println r)))


;; helpers
(defn switch-to-room
  [room target]
  (debug "MOVING FROM" room "TO" target)
  (screen! main-screen :switch-to-room :target target)
  nil)

(defn map->entity
  "I take a map and return an entity merged with the map."
  [m]
  (merge (texture (:image m)) m))

(defn room->entities
  "I make a vector? of entities from an entry in the rooms map."
  [r]
  (debug "CREATING ROOM FOR" r "...")
  (let [room (get rooms r)]
    (map #(do (debug "LOADING THINGIE" (:name %) "WITH VAL" %)
              (map->entity %))
         room)))


;; since all entities need a name, image, x, y, width and height....
;; note that the macro doesn't expand to an actual entity but to a map
(defmacro defentity
  [name image x y width height & m]
  `(assoc {}
          :name ~name
          :image ~image
          :x ~x :y ~y
          :width ~width :height ~height
          ~@m))


;; a simple macro to make arrow definitions more concise
(defmacro arrow [direction target]
  `(defentity
    ({:left "left" :right "right" :up "up" :down "down"} ~direction)
    ({:left "images/arrowLeft.png" :right "images/arrowRight.png" :up "images/arrowUp.png" :down "images/arrowDown.png"} ~direction)
    ({:left 50 :right 600 :up 316 :down 316} ~direction) ({:left 200 :right 200 :up 391 :down 25} ~direction) 64 64
    :action (fn
              [ent# screen# entities#]
              (switch-to-room (:current-room screen#) ~target))))

;; these macros capture the symbols ent, screen and entities so they
;; can be used in the body
(defmacro when-selected
  [item & body]
  `(fn ~['ent 'screen 'entities]
     (if (selected? ~'screen ~item)
       (do ~@body)
       ~'ent)))


;; small macro to make code more terse
;; may want to update the :image entry in the map, too?
(defmacro change-texture [who filename]
  `(texture! ~who :set-texture (texture! (texture ~filename) :get-texture)))


;; the game data
(def rooms {:room01
            [(defentity "" "images/room01.png" 0 0 700 480)
             (defentity "box" "images/box.png" 80 50 48 48
               :action (pickup-action-fn))
             (defentity "door" "images/door2.png" 250 81 128 256
               :action (when-selected "key"
                                      (change-texture ent "images/door2open.png")
                                      (assoc ent :action (fn [ent screen entities]
                                                           (switch-to-room (:current-room screen) :room05)))))
             (defentity "door" "images/door3.png" 12 16 35 256
               :action (when-selected "key"
                                      (change-texture ent "images/door3open.png")
                                      (assoc ent :action (fn [ent screen entities]
                                                           (switch-to-room (:current-room screen) :room05))))) ;; TODO: correct room here
             (arrow :right :room02)
             (arrow :down :room06)] ;; DEBUG ARROW

            :room02
            [(defentity "" "images/room02.png" 0 0 700 480)
             (hide (defentity "key" "images/key.png" 170 40 50 63
                     :action (pickup-action-fn)))
             (defentity "portrait" "images/portrait.png" 150 220 96 158
               :action (when-selected "knife"
                                      [(show (some (named "broken-portrait") entities))
                                       (show (some (named "key") entities))]))
             (hide (defentity "broken-portrait" "images/broken-portrait.png" 150 220 96 158))
             (hide (defentity "hammer" "images/hammer.png" 460 20 80 80
                     :action (pickup-action-fn)))
             (defentity "toolbox" "images/toolbox.png" 320 64 128 64
               :action (fn [ent screen entities]
                         (let [h (some (named "hammer") entities)]
                           (when h (pickup h screen entities)))
                         (dissoc ent :action)))
             (defentity "axe" "images/axe.png" 500 40 48 48
               :action (pickup-action-fn))
             (arrow :left :room01)
             (arrow :right :room03)]

            :room03
            [(defentity "" "images/room03.png" 0 0 700 480)
             (hide (defentity "knife" "images/knife.png" 380 80 64 64
                     :action (pickup-action-fn)))
             (hide (defentity "fire" "images/fire.png" 380 80 64 64
                     :action (when-selected "box"
                                            (consume "box" screen)
                                            (show (some (named "knife") entities)))))
             (hide (defentity "fireplace-logs" "images/logs.png" 380 80 61 61
                     :action (when-selected "lighter"
                                            (show (some (named "fire") entities)))))
             (defentity "fireplace" "images/fireplace.png" 250 81 303 253
               :action (when-selected "logs"
                                      (debug "PREPARING FIREPLACE")
                                      (consume "logs" screen)
                                      [(dissoc ent :action)
                                       (show (some (named "fireplace-logs") entities))]))
             (defentity "lighter" "images/lighter.png" 278 214 61 61
               :action (pickup-action-fn))
             (arrow :left :room02)
             (arrow :right :room04)]

            :room04
            [(defentity "" "images/room04.png" 0 0 700 480)
             (hide (defentity "logs" "images/logs.png" 450 30 61 61
                     :action (pickup-action-fn)))
             (defentity "bucket" "images/bucket.png" 70 40 48 48
                      :action (pickup-action-fn))
             (defentity "tree" "images/tree.png" 450 30 240 336
               :action (when-selected "axe"
                                      (debug "CUTTING TREE!")
                                      (show (some (named "logs") entities))))
             (arrow :left :room03)]

            :room05
            [(defentity "" "images/room05.png" 0 0 700 480)
             (hide (defentity "paper" "images/paper.png" 120 20 50 63
                     :action (pickup-action-fn)))
             (hide (defentity "bucket-in-table" "images/bucket.png" 470 130 50 63
                     :action (when-selected "paper"
                                            (consume "paper" screen)
                                            (assoc ent
                                                   :action (when-selected "lighter"
                                                                          [(dissoc ent :action)
                                                                           (show (some (named "ladder") entities))
                                                                           (show (some (named "smoke-in-bucket") entities))])))))
             (hide (defentity "smoke-in-bucket" "images/smoke.png" 470 181 48 48))
             (defentity "table" "images/table.png" 360 5 320 200
               :action (when-selected "bucket"
                                      (consume "bucket" screen)
                                      [(dissoc ent :action)
                                       (show (some (named "bucket-in-table") entities))]))
             (hide (defentity "ladder" "images/ladder.png" 160 30 60 770
                     :action (fn [ent screen entities]
                               (switch-to-room (:current-room screen) :room06))))
             (defentity "vase" "images/vase.png" 120 30 50 63
               :action (when-selected "hammer"
                                      (show (some (named "paper") entities))))
             (arrow :left :room01)]
            
            :room06
            [(hide (defentity "room06-bg" "images/room06.png" 0 0 700 480))
             (show (defentity "room06-bg-lamplit" "images/room06-lamplit.png" 0 0 700 480))
             (show (defentity "lantern-on-floor" "images/lantern.png" 145 60 48 48))
             
             (arrow :down :room05)
             ]})

;; main screen - where the game is played
(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (let [newroom (room->entities :room01)]
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
          (do (room->entities (:current-room screen)))
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
                        (room->entities target))]
      (update! screen
               :rooms (merge (screen :rooms)
                             {from entities}
                             {target newroom})
               :current-room target)
      newroom)))


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
