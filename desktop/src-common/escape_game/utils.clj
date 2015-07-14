(ns escape-game.utils
  (:require [escape-game.actions :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

(defn move
  [entity direction]
  (case direction
    :down (assoc entity :y (dec (:y entity)))
    :up (assoc entity :y (inc (:y entity)))
    :right (assoc entity :x (inc (:x entity)))
    :left (assoc entity :x (dec (:x entity)))
    nil))

(defn clicked?
  [ent screen]
  ;; FIXME: sorry, I don't have time to make it pretty :)
  (let [[x_ y_] [(game :x) (game :y)]
        m (input->screen screen x_ y_)
        [x y] [(:x m) (:y m)]]
    (and
     (< (:x ent) x (+ (:x ent) (:width ent)))
     (< (:y ent) (- 480 y) (+ (:y ent) (:height ent))))))

(defn update-inventory-coords
  [screen inv]
  (update! screen
           :select nil
           :inventory (map-indexed (fn [index ent]
                                     (assoc ent
                                            :x (+ 702 (* 49 (mod index 2)))
                                            :y (- 424 (* 60 (int (/ index 2))))))
                                   inv)))

(def debug? true) ;; umm................
(defn debug
  "I print debugging information if I should."
  [& r]
  (when debug? (apply println r)))


(defn switch-to-room
  [room target screen]
  (debug "MOVING FROM" room "TO" target)
  (screen! screen :switch-to-room :target target)
  nil)

(defn map->entity
  "I take a map and return an entity merged with the map."
  [m]
  (merge (texture (:image m)) m))

(defn room->entities
  "I make a vector? of entities from an entry in the rooms map."
  [r rooms]
  (debug "CREATING ROOM FOR" r "...")
  (let [room (get rooms r)]
    (map #(do (debug "LOADING THINGIE" (:name %) "WITH VAL" %)
              (map->entity %))
         room)))


(defn selected?
  [screen name]
  (= name (:name (:selected screen))))

(defn named
  [n]
  #(when (= n (:name %)) %))

(defn hide
  [ent]
  (assoc ent :hidden "bacon"))

(defn show
  [ent]
  (dissoc ent :hidden))

(defn pickup
  "I move an item from the screen to the inventory. and return nil so
  it doesn't get added back to the entities vector.  The new item will
  have an associated action that toggles its 'selected' property on
  and off."
  [ent screen entities]
  (let [inv (:inventory screen)
        n (count inv)
        new (assoc ent
                   :width 48 :height 48
                   :action (toggle-selected-fn))]
    (update-inventory-coords screen (concat (:inventory screen) [new])))
  nil)

(defn consume
  [name screen]
  (update-inventory-coords screen (remove (named name) (:inventory screen))))
