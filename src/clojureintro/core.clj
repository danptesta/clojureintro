(ns clojureintro.core
  (:use bakery.core))

(defn error [& args]
  (apply println args)
  :error)

(def scooped-ingredients #{:flour :milk :sugar})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn add-squeezed
  ([ingredient amount]
   (if (squeezed? ingredient)
     (do
       (dotimes [_ amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       :ok)
     (error "This function only works on squeezed ingredients.  You asked me to add " ingredient)))
  ([ingredient]
   (add-squeezed ingredient 1)))


(defn add-scooped
  ([ingredient amount]
   (if (scooped? ingredient)
     (do
       (dotimes [_ amount]
         (grab :cup)
         (scoop ingredient)
         (add-to-bowl)
         (release))
       :ok)
     (error "This function only works on scooped ingredients.  You asked me to add " ingredient)))
  ([ingredient]
   (add-scooped ingredient 1)))

(defn add-simple
  ([ingredient amount]
    (if (simple? ingredient)
      (do
        (dotimes [_ amount]
          (grab ingredient)
          (add-to-bowl))
       :ok)
      (error "This function only supports simple ingredients.  You asked me to add " ingredient)))
  ([ingredient]
    (add-simple ingredient 1)))

(defn add
  ([ingredient amount]
    (cond
      (squeezed? ingredient)
      (add-squeezed ingredient amount)
      (scooped? ingredient)
      (add-scooped ingredient amount)
      (simple? ingredient)
      (add-simple ingredient amount)
      :else
      (error "I do not have the ingredient" ingredient)))
  ([ingredient]
    (add ingredient 1)))

(defn bake-cake []
  (add :flour 2)
  (add :egg 2)
  (add :milk)
  (add :sugar)
  
  (mix)
  
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :sugar 1)
  (add :butter 1)

  (mix)

  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(def pantry-ingredients #{:flour :sugar})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def fridge-ingredients #{:egg :butter :milk})

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
    (fetch-from-pantry ingredient 1))
  ([ingredient amount]
    (if (from-pantry? ingredient)
      (do
        (go-to :pantry)
        (dotimes [i amount]
          (load-up ingredient))
        (go-to :prep-area)
        (dotimes [i amount]
          (unload ingredient)))
      (error "This function only works on ingredients stored in the pantry.  You asked me to fetch " ingredient))))

(defn fetch-from-fridge
  ([ingredient]
    (fetch-from-fridge ingredient 1))
  ([ingredient amount]
    (if (from-fridge? ingredient)
      (do
        (go-to :fridge)
        (dotimes [i amount]
          (load-up ingredient))
        (go-to :prep-area)
        (dotimes [i amount]
          (unload ingredient)))
      (error "This function only works on ingredients stored in the fridge.  You asked me to fetch " ingredient))))

(defn fetch-ingredient
  ([ingredient]
    (fetch-ingredient ingredient 1))
  ([ingredient amount]
    (cond
      (from-pantry? ingredient)
      (fetch-from-pantry ingredient amount)
      (from-fridge? ingredient)
      (fetch-from-fridge ingredient amount)
      :else
      (error "I don't know where to get " ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-list [shopping-list]
  (doseq [[location ingredients] {:pantry pantry-ingredients
                                  :fridge fridge-ingredients}]
    (go-to location)
    (doseq [ingredient ingredients]
      (load-up-amount ingredient (shopping-list ingredient 0))))

  (go-to :prep-area)
  (doseq [[ingredient amount] shopping-list]
    (unload-amount ingredient amount)))

(def cake-ingredients {:egg 2
                       :flour 2
                       :milk 1
                       :sugar 1})

(def cookies-ingredients {:egg 1
                          :flour 1
                          :butter 1
                          :sugar 1})

(defn add-ingredients [list1 list2]
  (merge-with + list1 list2))

(defn multiply-ingredients [quantity ingredients]
  (into {} 
    (for [[ingredient amount] ingredients]
      [ingredient (* quantity amount)])))

(defn order->ingredients [order]
  (add-ingredients
    (multiply-ingredients (:cake (:items order) 0) cake-ingredients)
    (multiply-ingredients (:cookies (:items order) 0) cookies-ingredients)))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))

(defn bake [item]
  (cond
  (= item :cake)
  (bake-cake)
  (= item :cookies)
  (bake-cookies)
  :else
  (error "I don't know how to bake " item)))

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
