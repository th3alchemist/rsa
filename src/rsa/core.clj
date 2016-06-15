(ns rsa.core)

(defn exp [x n] (apply *' (take n (repeat x))))

(defn encrypt [[m n] e]
  (mod (exp m e) n))

(defn decrypt [[c n] d]
  (mod (exp c d) n))

(defn extended-euclidean-algorithm [a b]
  (loop [quotient [0] remainder [a b]]
    (let [previous (nth remainder (- (count remainder) 1))
          before-previous (nth remainder (- (count remainder) 2))]
      (if (zero? (last remainder))
        {:quotient quotient :remainder remainder :gcd (peek (pop remainder))}
        (recur (conj quotient
                     (quot before-previous previous))
               (conj remainder
                     (rem before-previous previous)))))))

(defn back-substitution [quotient-vec]
  (loop [i 2 x [1 0] y [0 1]]
    (let [previous (- i 1) before-previous (- i 2)]
      (if (= i (count quotient-vec))
        {:x x :y y :d (last x)}
        (recur (inc i)
               (conj x
                     (- (nth x before-previous)
                        (* (nth quotient-vec previous)
                           (nth x previous))))
               (conj y
                     (- (nth y before-previous)
                        (* (nth quotient-vec previous)
                           (nth y previous)))))))))

(defn generate-private-key [e phi];e is the public key, prime number between 3 and phi
  (let [d (:d (back-substitution (:quotient (extended-euclidean-algorithm e phi))))]
    (if (neg? d)
      (+ d (* (inc (quot (* -1 d) phi)) phi))
      d)))

(defn rsa [p q]
  {:n (* p q) :phi (* (- p 1) (- q 1))})
;http://doctrina.org/How-RSA-Works-With-Examples.html

(defn gcd [a b]
  (:gcd (extended-euclidean-algorithm a b)))