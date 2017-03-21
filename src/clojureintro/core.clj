(ns clojureintro.core
  (:use bakery.core))

(def baking {:recipes {:cake {:ingredients {:egg 2
                                            :flour 2
                                            :sugar 1
                                            :milk 1}
                              :steps [[:add :all]
                                      [:mix]
                                      [:pour]
                                      [:bake 25]
                                      [:cool]]}
                      :cookies {:ingredients {:egg 1
                                              :flour 1
                                              :sugar 1
                                              :butter 1}
                                :steps [[:add :all]
                                        [:mix]
                                        [:pour]
                                        [:bake 30]
                                        [:cool]]}
                      :brownies {:ingredients {:butter 2
                                               :sugar 1
                                               :cocoa 2
                                               :flour 2
                                               :egg 2
                                               :milk 1}
                                 :steps [[:add :butter]
                                         [:add :sugar]
                                         [:add :cocoa]
                                         [:mix]
                                         [:add :flour]
                                         [:add :egg]
                                         [:add :milk]
                                         [:mix]
                                         [:pour]
                                         [:bake 35]
                                         [:cool]]}}
             :ingredients {:egg {:storage :fridge
                                 :usage :squeezed}
                           :milk {:storage :fridge
                                  :usage :scooped}
                           :butter {:storage :fridge
                                    :usage :simple}
                           :flour {:storage :pantry
                                   :usage :scooped}
                           :sugar {:storage :pantry
                                   :usage :scooped}
                           :cocoa {:storage :pantry
                                   :usage :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                        (scoop ingredient)
                        (add-to-bowl))
                       (release))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))})

(defn add
  ([ingredient amount]
    (let [u (:usage (ingredient (:ingredients baking)))
          f (u usage)]
      (f ingredient amount)))
  ([ingredient]
    (add ingredient 1)))

(def actions {:cool (fn [recipe] (cool-pan))
              :mix (fn [recipe] (mix))
              :pour (fn [recipe] (pour-into-pan))
              :bake (fn [recipe minutes] (bake-pan minutes))
              :add (fn 
                    ([recipe ingredient]
                      (cond
                        (= [:all] ingredient)
                        (doseq [[ingredient amount] (:ingredients recipe)]
                          (add ingredient amount))
                        (contains? (:ingredients recipe) ingredient)
                        (add ingredient (ingredient (:ingredients recipe)))))
                      ([recipe ingredient amount]
                        (add ingredient amount)))})

(defn error [& args]
  (apply println args)
  :error)

(defn perform [recipe step]
  (let [f (actions (first step))]
        (apply f recipe (rest step))))

(defn bake-recipe [recipe]
  (last (for [step (:steps recipe)]
    (perform recipe step))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn map-storage [shopping-list]
  (for [[ingredient amount] shopping-list]
                         {:ingredient ingredient
                          :amount amount
                          :storage (:storage (ingredient (:ingredients baking)))}))

(defn load-ingredients [ingredients]
  (doseq [ingredient ingredients]
    (load-up-amount (:ingredient ingredient) (:amount ingredient))))

(defn unload-ingredients [shopping-list]
  (doseq [[ingredient amount] shopping-list]
    (unload-amount ingredient amount)))

(defn fetch-list [shopping-list]
  (let [with-storage (map-storage shopping-list)]
    (doseq [[location ingredients] (group-by :storage with-storage)]
      (go-to location)
      (load-ingredients ingredients))
    (go-to :prep-area)
    (unload-ingredients shopping-list)))

(defn add-ingredients [list1 list2]
  (merge-with + list1 list2))

(defn multiply-ingredients [quantity ingredients]
  (into {} 
    (for [[ingredient amount] ingredients]
      [ingredient (* quantity amount)])))

(defn order->ingredients [order]
  (reduce add-ingredients (for [[item amount] (:items order)]
                            (multiply-ingredients amount
                                                  (:ingredients (item (:recipes baking)))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))

(defn bake [item]
  (bake-recipe ((:recipes baking) item)))

(defn bake-order [order]
  (for [[item amount] (:items order)
         i (range amount)]
    (bake item)))

(defn bake-orders [orders]
  (doseq [order orders]
    (let [racks (bake-order order)
          receipt {:orderid (:orderid order)
                   :address (:address order)
                   :rackids racks}]
      (delivery receipt))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredient-list (orders->ingredients orders)]
    (fetch-list ingredient-list)
    (bake-orders orders)))

(defn -main [] 
  (day-at-the-bakery))
