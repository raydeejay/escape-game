(ns escape-game.actions
  (:require [play-clj.core :refer :all]
            [escape-game.utils :refer :all]))

(defmacro pickup-action-fn []
  `(fn [ent# screen# entities#]
     (pickup (hide ent#) screen# entities#)))

