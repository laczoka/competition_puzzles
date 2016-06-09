(ns competition-puzzles.space-invaders)

;; The Space Invaders are preparing to conquer new, belligerent galaxies.
;; The emperor has instructed his four chief admirals to prepare for an invasion of K selected galaxies.
;; All four are eager to earn more power and glory for their imperium, however, in order to be successful,
;; they need to cooperate.

;; The emperor fleet of N divisions, each division has Ai number of starfighters in it. For a successful invasion of a galaxy,
;; each admiral gets to command a starfighter detachment of equal size, otherwise they will lose the battle.
;; They may get a 0 size detachment of starfighters, in this case they only fight the battle with their flagship.

;; The following rules need to be observer when assembling the detachments:
;; - A subsequence of K divisions must be selected (from the N number of total divisions in the fleet)
;; - The j. battle will have a squad of starfighters from the j. selected division such that its size is divisible by 4.

;; The starfighters within a division have unique strengths. For a division of size 6, the detachment of starfighters {0,1,2,3}
;; is different from the detachment of starfighters {3,4,5,6}. 

;; The emperor needs to know the number of ways of selecting K detachments of divisions to conquer K galaxies using the criterion above.
;; As this number may be quite large, print the answer modulo 10^9 + 7.


;; fleet : vec of ints

;; ( N over K) number of different selection of divisions

;; within a division combination:
;;
;;      j=Ai div 4
;; Ci = SUM( ( Ai choose (4*j) )
;;      j=0

;; we rearrange the computation steps to make multiple passes over C
;; so would need to compute (A1 choose k), (A2 choose k) for k = 0,4,8 .. Ai div 4
;; for this we need to compute diagonals of the Pascal triangle, for which formulas exist:
;; (n+i choose i) = (n+i-1 choose i-1) * (n+i/i)

;; Issue: Ai can be very large

;; Solution:

;; Sum of every nth binomial coeff:
;; http://math.stackexchange.com/questions/142260/sum-of-every-kth-binomial-coefficient
;; https://www.math.hmc.edu/~benjamin/papers/EvenlySpacedBinomialsMag.pdf
;; SUM (n choose 4*j) = (2^n + m*2^(ceil(n/2))/4
;; where m = 2, 1, 0, −1, −2, −1, 0, 1, when n ≥ 1 is congruent, respectively, to
;; 0, 1, 2, 3, 4, 5, 6, 7 (mod 8).
;; (When n = 0, this formula needs to be adjusted, since 0^0 = 1)
;; (set! *warn-on-reflection* true)

(def ^:const M (+ 1000000000 7))

(defn mod-M ^long [^long n] (mod n ^long M))

(def m-coeff (int-array [2 1 0 -1 -2 -1 0 1]))

;;(2^(n-2) + m * 2^(ceil(n/2) - 2))
(defn sum-4jth-binom-coeffs ^long [^long n]
  (let [M (BigInteger/valueOf M)
        ceil-n-div-2 (+ (quot n 2) (mod n 2))
        m (aget ^ints m-coeff (mod n 8))]
    (.longValue
      (.mod
        (.add (.modPow (BigInteger/valueOf 2) (BigInteger/valueOf (- n 2)) M)
              (.mod (.multiply (BigInteger/valueOf m) (.modPow (BigInteger/valueOf 2) (BigInteger/valueOf (- ceil-n-div-2 2)) M)) M))
        M))))

(require '[clojure.string :refer [split]])

;; Computing the sum of products of all k-size subsets
;; using a dynamic programming formula
(defn sum-of-products-of-k-subsets [^long K ^longs C]
  (let [N (alength C)
        Sn-1 (long-array (inc N))
        Sn (long-array (inc N))]
    ;; init
    (aset Sn-1 0 (long 1))
    (aset Sn 0 (long 1))
    ;; n: 1->N
    (loop [Sn-1 Sn-1
           Sn Sn
           n (int 1)
           result (long -1)]
      (if (<= n N)
        ;; k: 1->max(K,n)
        (let [result (loop [k (int 1)]
                       (if (and (<= k K) (<= k n))
                         ;; continue iteration
                         (let [Snk (mod-M (+ (aget Sn-1 k) (* (aget Sn-1 (dec k)) (aget C (dec n)))))] ;; C is 0-based
                           (aset Sn k Snk)
                           (recur (inc k)))
                         ;; result computed in the previous step
                         (aget Sn (dec k))))]
          (recur Sn Sn-1 (int (inc n)) (long result)))
        ;; result
        result))))

(defn space-invaders []
  (let [[_ K] (map #(Long/parseLong %) (split (read-line) #"\s+"))
        A (long-array (map #(Long/parseLong %) (split (read-line) #"\s+")))
        C (long-array (map sum-4jth-binom-coeffs A))
        D (sum-of-products-of-k-subsets K C)]
    (println D)))

;; (space-invaders)


(with-open [is (clojure.java.io/reader "test/space_invaders/inp_03.txt")]
  (binding [*in* is]
    (space-invaders)))
