(ns escape-game.actions
  (:require [play-clj.core :refer :all]))

(defmacro pickup-action-fn []
  `(fn [ent# screen# entities#]
     (escape-game.utils/pickup (escape-game.utils/hide ent#) screen# entities#)))

(defmacro toggle-selected-fn []
  `(fn [ent# screen# entities#]
     (if (= (:selected screen#) ent#)
       (do (escape-game.utils/debug "DESELECTING" (:name ent#))
         (update! screen# :selected nil))
       (do (escape-game.utils/debug "SELECTING" (:name ent#))
         (update! screen# :selected ent#)))))

