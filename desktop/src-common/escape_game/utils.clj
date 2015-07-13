(ns escape-game.utils
  (:require [play-clj.core :refer :all]))

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
                   :action (fn [ent_ screen_ entities_]
                             (println "SELECTING" (:name ent_))
                             (let [new_ (assoc ent_ :selected (not (:selected ent_)))]
                               (update! screen :selected new_)
                               new_)))]
    (update-inventory-coords screen (concat (:inventory screen) [new])))
  nil)

(defn consume
  [name screen]
  (update-inventory-coords screen (remove (named name) (:inventory screen))))

