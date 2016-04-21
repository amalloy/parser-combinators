(ns parser-combinators.core)

;; Monad m => a -> m a
(defn return [x]
  (fn [s]
    [:success [x, s]]))

;; Monad m => m a -> (a -> m b) -> m b
(defn bind [p f]
  (fn [s]
    (let [[result [value remainder] :as r] (p s)]
      (if (= result :failure)
        r
        (let [p' (f value)]
          (p' remainder))))))

;; Monad m => (a -> b) -> m a -> m b
(defn fmap [f p]
  (bind p (fn [v] (return (f v)))))

;; Parser a
(defn fail [msg]
  (fn [s]
    [:failure msg]))

;; Parser ()
(defn succeed []
  (fn [s]
    [:success [() s]]))

;; Char -> Parser Char
(defn lit [x]
  (fn [s]
    (cond (empty? s) [:failure "empty"]
          (= x (first s)) [:success [x (rest s)]]
          :else [:failure "no match"])))

;; Parser a -> Parser b -> Parser b
(defn conc' [a b]
  (bind a
        (fn [_]
          b)))

(defn conc-monadic [a b f]
  (bind a
        (fn [x]
          (bind b
                (fn [y]
                  (return (f x y)))))))

;; Parser a -> Parser b -> (a -> b -> c) -> Parser c
(defn conc [a b f]
  (fn [s]
    (let [[result [value remainder] :as r] (a s)]
      (if (= result :failure)
        r
        (let [[result2 [value2 remainder2] :as r2] (b remainder)]
          (if (= result :failure)
            r2
            [:success [(f value value2) remainder2]]))))))

;; Parser a -> Parser a -> Parser a
(defn alt [a b]
  (fn [s]
    (let [r (a s)
          [result [value remainder]] r]
      (if (= result :success)
        r
        (b s)))))

;; Parser ()
(defn empty []
  (fn [s]
    (if (empty? s)
      [:success [() s]]
      [:failure "not empty"])))

(defn forever [x]
  (bind (lit x) (fn [_]
                  (forever x))))

;; (declare exp)
;; (def exp' (alt (conc (lit '+') exp (fn [_ x] (fn [y] (+ (parse x) y)))) ))
;; ((fn [] )) (def exp (conc term exp'))


;; (declare y)
;; (defn x (fn [] (+ 1 y)))
;; (def y 2)

(def fibs (list* 0 1 (map + fibs (rest fibs))))
