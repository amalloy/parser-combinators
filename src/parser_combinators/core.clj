(ns parser-combinators.core)

;; Monad m => a -> m a
(defn return [x]
  (fn [s k]
    (k [:success [x, s]])))

;; Monad m => m a -> (a -> m b) -> m b
(defn bind [p f]
  (fn [s k]
    (p s (fn [[result [value remainder] :as r]]
           (if (= result :failure)
             (k r)
             (let [p' (f value)]
               (p' remainder k)))))))

;; Monad m => (a -> b) -> m a -> m b
(defn fmap [f p]
  (bind p (fn [v] (return (f v)))))

;; Parser a
(defn fail [msg]
  (fn [s k]
    (k [:failure msg])))

;; Parser ()
(defn succeed []
  (fn [s k]
    (k [:success [nil s]])))

;; Char -> Parser Char
(defn lit [x]
  (fn [s k]
    (k (cond (empty? s) [:failure "empty"]
             (= x (first s)) [:success [x (rest s)]]
             :else [:failure "no match"]))))

;; Parser a -> Parser b -> Parser b
(defn conc' [a b]
  (bind a
        (fn [_]
          b)))

(defn conc [a b f]
  (bind a
        (fn [x]
          (bind b
                (fn [y]
                  (return (f x y)))))))

#_(defn conc-do [a b f]
  (do [x a
       y b]
      (f x y)))

;; Parser a -> Parser a -> Parser a
(defn alt [a b]
  (fn [s k]
    (a s (fn [[result [value remainder] :as r]]
           (if (= result :success)
             (let [[result [value remainder] :as r] (k r)]
               (if (= result :success)
                 r
                 (b s k)))
             (b s k))))))

;; Parser ()
(defn empty []
  (fn [s k]
    (k (if (empty? s)
         [:success [nil s]]
         [:failure "not empty"]))))

(defn forever [x]
  (bind (lit x) (fn [_]
                  (forever x))))

(defmacro defer [parser]
  `(fn [s#] (~parser s#)))

;; A = abAA | ''

abab

A -> abAA -> abA'' -> ababAA'' -> abab''''''
A -> abAA -> ab''A -> ab''abAA -> ab''ab''''

(def A (alt (succeed)
            (conc #'A
                  (conc (lit \a)
                        (lit \b)
                        str)
                  str)))
(def S (conc #'A (empty) (fn [a b] a)))

[""
 "ab"
 "abab"
 "ababab"] ;;not cccdfvababab


(defn many [p]
  (alt (bind p
             (fn [v]
               (fmap #(cons v %) (many p))))
       (succeed)))
