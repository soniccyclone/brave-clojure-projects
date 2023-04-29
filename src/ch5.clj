(ns ch5
  (:require [clojure.string :as s]))

(defn hello
  []
  "hello dude")

(defn wisdom
  [words]
  (str words ", Daniel-san"))

(defn year-end-evaluation
  []
  (if (> (rand) 0.5)
    "You get a raise!"
    "Better luck next year!"))

(defn analysis
  [text]
  (str "Character count: " (count text)))

(defn sum
  ([vals]
   (sum vals 0))
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total
     (recur (rest vals) (+ (first vals) accumulating-total)))))

(defn clean
  [text]
  (s/replace (s/trim text) #"lol" "LOL"))

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

(defn spell-slots
  [char]
  (int (inc (/ (c-int char) 2))))

(def spell-slots-comp (comp int inc #(/ % 2) c-int))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

;; (defn my-comp
;;   [& g]
;;   (if (= (count g) 1)
;;     (fn [& args]
;;       (apply (first g) args))
;;     ((first g) (my-comp (rest g)))))

(defn my-comp
  [& b]
  (fn [& args]
    (defn inner-comp
      ;; Silly me, I can't destructure the params again here because we've turned the params list into a proper sequence at this point!!
      [g]
      (if (= (count g) 1)
        (apply (first g) args)
        ((first g) (inner-comp (rest g)))))
    (inner-comp b)))

(defn my-comp-recur
  [& g]
  (fn [& args]
    (defn inner-comp-recur
      [g accumulate]
      (if (= (count g) 1)
        ((first g) accumulate)
        (recur (rest g) ((first g) accumulate))))
    (let [b (reverse g)]
      (inner-comp-recur (rest b) (apply (first b) args)))))