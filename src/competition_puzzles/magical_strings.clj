(ns puzzles.magical-strings)

;; Credits: Puzzle taken from a programming competition site. If you happen to recognise it, please
;; do not submit it as your own solution as it is plagiarism.

;; Given a string of lower case english letters (S) of length N
;; The goal is to compute the number of distinct subsequences of S of length N-2

;; A straight forward analysis of the combinatoric structure doesn't lead to the correct result
;; due to number of possible overlapping cases which are difficult to account for

;; Given finite (and small alphabet) one can compute the result in linear polynomial time
;; using a recurrence relation
;; http://www.cs.ucr.edu/~stelo/cpm/cpm06/12-rahmann.pdf

;; Optimized for speed and to minimize allocation but still somewhat naive.
;; We could further cut down on memory allocation, as we only every use N-K+1 slots of the C1 and C2 row arrays

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn distinct-k-subseq-of [K ^String S]
  (let [alphabet (into (vector-of :char) (set (seq S)))
        Ca (count alphabet)
        a-step (long (+ 1 Ca))
        N (.length S)
        K (long K)
        C1 (long-array (* (+ N 1) a-step))
        C2 (aclone C1)]
    (println (.charAt S 0))
    (println (.charAt S (dec N)))
    (aset C1 0 1)                                     ;; init
    (loop [j 1
           C_j-1 C1
           C_j C2]
      (if (<= j N)
        (do
          (aset C_j 0 1)                              ;; j. step, m=0 init
          (loop [m (* (max 1 (- j 2)) a-step)]        ;; j. step, m=1
            ;; run through the alphabet
            (when (< m (min (* (inc j) a-step) (alength C_j)))
              (aset C_j m
                    (long (loop [ai 1
                                 sum 0]
                            (if (< ai a-step)
                              (let [mja (+ m ai)
                                    c_mja (if (= ^char (alphabet (- ai 1)) (.charAt S (- j 1)))
                                            ;; formula 1. -> "append char"
                                            (aget C_j-1 (- m a-step))
                                            ;; formula 2. -> "inherit"
                                            (aget C_j-1 mja))]
                                ;; (println mja "<-" c_mja)
                                (recur (+ ai 1) (+ sum (aset C_j mja c_mja))))
                              ;; else return sum
                              sum))))
              (recur (+ m a-step))))
          (recur (+ j 1) C_j C_j-1))                        ;; swap C_j and C_j-1
        ;; else -> we already computed C_K_N
        (aget C_j-1 (* K a-step))))))

(defn main [f]
  (let [S ^String (f)]
    (println (distinct-k-subseq-of (- (.length S) 2) S))))

;; (main (fn [] "abba"))
;; (main read-line)
;; (main (partial slurp "test/competition_puzzles/magical_strings/inp04.txt"))

