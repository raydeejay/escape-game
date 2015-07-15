(ns escape-game.gamedata
  (:require [escape-game.utils :refer :all]
            [escape-game.actions :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

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
              (switch-to-room (:current-room screen#) ~target screen#))))

;; this macro captures the symbols ent, screen and entities so they
;; can be used in the body
(defmacro when-selected
  [item & body]
  `(fn ~['ent 'screen 'entities]
     (if (selected? ~'screen ~item)
       (do ~@body)
       ~'ent)))


(defn change-texture [who filename]
  (-> who (dissoc :object) (assoc :image filename) (map->entity)))


;; the game data
(def rooms {:room01
            [(defentity "" "images/room01.png" 0 0 700 480)
             (defentity "box" "images/box.png" 80 50 48 48
               :action (pickup-action-fn))
             (defentity "door" "images/door2.png" 250 81 128 256
               :action (when-selected "key"
                                      (assoc (change-texture ent "images/door2open.png")
                                             :action (fn [ent screen entities]
                                                       (switch-to-room (:current-room screen) :room05 screen)))))
             (defentity "door" "images/door3.png" 12 16 35 256
               :action (when-selected "key"
                                      (assoc (change-texture ent "images/door3open.png")
                                             :action (fn [ent screen entities]
                                                       (switch-to-room (:current-room screen) :room05 screen))))) ;; TODO: switch to stairs
             (arrow :right :room02)]

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
                               (switch-to-room (:current-room screen) :room06 screen))))
             (defentity "vase" "images/vase.png" 120 30 50 63
               :action (when-selected "hammer"
                                      (show (some (named "paper") entities))))
             (arrow :left :room01)]

            :room06
            [(hide (defentity "room06-bg" "images/room06.png" 0 0 700 480))
             (hide (defentity "room06-bg-lamplit" "images/room06-lamplit.png" 0 0 700 480))
             (hide (defentity "lantern-on-floor" "images/lantern.png" 145 60 48 48))

             (arrow :down :room05)
             ]})
