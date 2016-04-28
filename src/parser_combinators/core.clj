(ns parser-combinators.core)

;; Monad m => a -> m a
(defn return [x]
  (fn [s]
    [[x, s]]))

;; Monad m => m a -> (a -> m b) -> m b
(defn bind [p f]
  (fn [s]
    (for [[v more] (p s)
          v' ((f v) more)]
      v')))

;; Monad m => (a -> b) -> m a -> m b
(defn fmap [f p]
  (bind p (fn [v] (return (f v)))))

;; Parser a
(defn fail [msg]
  (fn [s]
    []))

;; Parser ()
(defn succeed []
  (fn [s]
    [[nil s]]))

;; Char -> Parser Char
(defn lit [x]
  (fn [s]
    (cond (empty? s) []
          (= x (first s)) [[x (rest s)]]
          :else [])))

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
  (fn [s]
    (concat (a s) (b s))))

;; Parser ()
(defn empty []
  (fn [s]
    (when (empty? s)
      [[nil s]])))

(defn forever [x]
  (bind (lit x) (fn [_]
                  (forever x))))

(defmacro defer [parser]
  `(fn [s#] (~parser s#)))

;; A = '' | Aab

(def A (alt (succeed)
            (conc #'A
                  (conc (lit \a) (lit \b) str)
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
