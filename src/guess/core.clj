(ns guess.core
  (:require [clojure.string :as str])
  (:gen-class))


(def init-state {:word "" :letters-correct [] 
                 :letters-wrong [] :guess ""})
(defn- guess-letter
  [l state]
  (let [word (:word state) size (count l) c (first l)]
    (if (= size 1)
      (if (some #{(first l)} word)
        (update state :letters-correct (fn [x] (conj x c)))
        (update state :letters-wrong (fn [x] (conj x c))))
      (update state :guess (fn [_] l)))))


(defn- check-word
  [state]
  (let [word (:word state) guess (:guess state)]
    (= word guess)))

(defn- word-to-dots
  [state]
  (let [w (seq (:word state)) 
        correct-letters (:letters-correct state)]
    (apply str (map (fn [x] (if (some #{x} correct-letters)
                             x
                             \.)) w))))
    
(defn- set-word-from-file
  [length file-name]
  (let [list (slurp "dutch.dic")]
   (update 
     init-state 
     :word (fn [_] 
             (rand-nth 
               (filter #(< (count %1) 7)
                       (clojure.string/split list #"\n")))))))


(defn- gameloop
  [run state]
  ;(println (:word state))
  (println (word-to-dots state))
  (if (not (check-word state))
    (recur true (guess-letter (read-line) state))
    (println "Well done! The word was" (:word state))))


(defn -main
  "I don't do a whole lot ... yet."
  []
  (let [state (set-word-from-file 7 "dutch.dic")]
       (gameloop true state)))
