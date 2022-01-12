(defn many-op [operation]
      (fn [& args]
          (apply mapv operation args)))

(def v+ (many-op +))
(def v- (many-op -))
(def v* (many-op *))
(def vd (many-op /))

(defn run-op [operation]
      (fn [o s]
          (mapv (fn [x] (operation x s)) o)))

(def v*s (run-op *))
(def m*s (run-op v*s))

(defn scalar ([a b] (apply + (v* a b))))

(defn vect
      ([[a b c] [x y z]]
       [(- (* b z) (* c y))
        (- (* c x) (* a z))
        (- (* a y) (* b x))]))

(def m+ (many-op v+))
(def m- (many-op v-))
(def m* (many-op v*))
(def md (many-op vd))

(def m*v (run-op scalar))

(defn transpose [a] (apply mapv vector a))

(defn m*m
      ([a b]
       (let [tb (transpose b)]
            (mapv
              (fn [n] ((run-op scalar) tb n))
              a
              ))))

(defn many-deep-op [op]
      (fn calc [a b]
          (if (vector? a)
            (mapv calc a b)
            (op a b))))

(def t+ (many-deep-op +))
(def t- (many-deep-op -))
(def t* (many-deep-op *))
(def td (many-deep-op /))