(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b e]
                 (if (>= 0 e)
                   b
                   (recur (* b base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond (== 0 (count a-seq)) nil
        (== 1 (count a-seq)) (first a-seq)
        :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (== 0 (count seq1)) (== 0 (count seq2))) true
        (and (== (count seq1) (count seq2)) (== (first seq1) (first seq2))) (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         ind 0]
    (if (empty? s)
      nil
      (if (pred (first s))
        ind
        (recur (rest s) (inc ind))))))

(defn avg [a-seq]
  (loop [s a-seq
         sum 0
         cnt 0]
    (if (empty? s)
      (/ sum cnt)
      (recur (rest s) (+ sum (first s)) (inc cnt)))))

(defn parity [a-seq]
  (loop [s a-seq
         result #{}]
    (if (empty? s)
      result
      (if (contains? result (first s))
        (recur (rest s) (disj result (first s)))
        (recur (rest s) (conj result (first s)))))))

(defn fast-fibo [n]
  (loop [Fn-1 0
         Fn 1
         N n]
    (if (== 0 N)
      Fn-1
      (recur Fn (+ Fn Fn-1) (dec N)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         seen #{}
         result []]
    (if (or (empty? s) (contains? seen (first s)))
      result
      (recur (rest s) (conj seen (first s)) (conj result (first s))))))

