(ns clojure-bst.core
  ^{:doc
      "Balanced binary search tree library (unfinished)."
      :author "Jake Piccolo"})

(defn- tree-member? [{:keys [data left right]} x]
  (cond
    (< x data) (tree-member? left x)
    (> x data) (tree-member? right x)
    :else true))

(defn matches? [match-expr expr]
  (cond
    (= match-expr :_) true
    (coll? match-expr) (apply (first match-expr) expr (rest match-expr))
    :else (match-expr expr)))

(defn all-match? [match-exprs exprs]
  (every? true? (map matches? match-exprs exprs)))

(defprotocol BinarySearchTree
  (is-empty? [this])
  (insert [this x])
  (delete [this x])
  (member? [this x]))

(defrecord RedBlackTree [color left data right])

(defrecord AVLTree [height left data right])

(declare avl-insert avl-delete rbt-insert)

(extend-protocol BinarySearchTree
  nil
  (is-empty? [_] true)
  (insert [_ _] (throw (NullPointerException.)))
  (delete [this _] this)
  (member? [_ _] false)

  AVLTree
  (is-empty? [_] false)
  (insert [this x] (avl-insert this x))
  (delete [this x] (avl-delete this x))
  (member? [this x] (tree-member? this x))

  RedBlackTree
  (is-empty? [_] false)
  (insert [this x]
    (let [{:keys [left data right]} (rbt-insert this x)]
      (->RedBlackTree :black left data right)))
  (delete [this x] ,,,) ; TODO
  (member? [this x] (tree-member? this x)))

(defn height [x]
  (cond
    (nil? x) 0
    (contains? x :height) (:height x)
    :else (inc (max (height (:left x)) (height (:right x))))))

(defn tree-min [tree]
  (->> (reductions get tree (repeat :left))
       (take-while (complement nil?))
       last
       :data))


;;;;;;;; AVL implementations ;;;;;;;;;
(defn avl [left data right]
  (let [height* (height {:left left, :data data, :right right})]
    (->AVLTree height* left data right)))

(defn- avl->vec [tree]
  ((juxt :height :left :data :right) tree))

(defn- rotate [dir tree]
  (let [[_ left data right] (avl->vec tree)
        [_ ll ld lr] (avl->vec left)
        [_ rl rd rr] (avl->vec right)]
    (condp = dir
      :left (avl (avl left data rl) rd rr)
      :right (avl ll ld (avl lr data right)))))

(defn balance-factor [{:keys [left right] :as tree}]
  (- (height left) (height right)))

(defn- avl-balance [{:keys [left data right] :as tree}]
  (condp all-match? (mapv balance-factor [left tree right])
    [[< 0] [> 1]  :_   ] (rotate :right (avl (rotate :left left) data right))
    [:_    [> 1]  :_   ] (rotate :right tree)
    [:_    [< -1] [> 0]] (rotate :left (avl left data (rotate :right right)))
    [:_    [< -1] :_   ] (rotate :left tree)
    tree))

(defn- avl-insert [{:keys [left data right] :as tree} x]
  (cond
    (is-empty? tree) (avl nil x nil)
    (< x data) (avl-balance (avl (avl-insert left x) data right))
    (> x data) (avl-balance (avl left data (avl-insert right x)))
    :else tree))

(defn avl-remove-node [{:keys [left right]}]
  (condp all-match? [left right]
    [nil? nil?] nil   ; No children
    [nil? :_  ] right ; Right child only
    [:_   nil?] left  ; left child only
    [:_   :_  ] (let [successor (tree-min right)]
                  (avl left successor (delete right successor)))))

(defn avl-delete [{:keys [left data right] :as tree} x]
  (cond
    (< x data) (avl-balance (avl (delete left x) data right))
    (> x data) (avl-balance (avl left data (delete right x)))
    :else (avl-remove-node tree)))

; #_(defn- avl-balance [left data right]
;   (let [tree (avl left data right)]
;     (condp all-match? (mapv balance-factor [left tree right])
;       [[< 0] [> 1]  :_   ] (rotate :right (avl (rotate :left left) data right))
;       [:_    [> 1]  :_   ] (rotate :right tree)
;       [:_    [< -1] [> 0]] (rotate :left (avl left data (rotate :right right)))
;       [:_    [< -1] :_   ] (rotate :left tree)
;       tree)))

; #_(defn- insert* [cons-fn bal-fn {:keys [left data right] :as tree} x]
;   (cond
;     (is-empty? tree) (cons-fn nil x nil)
;     (< x data) (bal-fn (insert cons-fn bal-fn left x) data right)
;     (> x data) (bal-fn left data (insert* cons-fn bal-fn right x))
;     :else tree))

; #_(defn- avl-insert [tree x]
;   (insert* avl avl-balance tree x))


;;;;; Red-black implementations ;;;;;;;
(def rbt ->RedBlackTree)

(defn- rbt->vec [tree]
  ((juxt :color :left :data :right) tree))

(defn- rbt-balance [color left data right]
  (let [[lc ll ld lr] (rbt->vec left)
        [llc lll lld llr] (rbt->vec ll)
        [lrc lrl lrd lrr] (rbt->vec lr)
        [rc rl rd rr] (rbt->vec right)
        [rlc rll rld rlr] (rbt->vec rl)
        [rrc rrl rrd rrr] (rbt->vec rr)
        red (partial rbt :red)
        black (partial rbt :black)]
    (condp #(and (= %1 %2) (= color :black)) [:red :red]
      [lc llc] (red (black lll lld llr) ld (black lr data right))
      [lc lrc] (red (black ll ld lrl) lrd (black lrr data right))
      [rc rlc] (red (black left data rll) rld (black rr rd rr))
      [rc rrc] (red (black left data rl) rd (black rrl rrd rrr))
      (rbt color left data right))))

(defn- rbt-insert [{:keys [color left data right] :as tree} x]
  (cond
    (is-empty? tree) (->RedBlackTree :red nil x nil)
    (< x data) (rbt-balance color (rbt-insert left x) data right)
    (> x data) (rbt-balance color left data (rbt-insert right x))
    :else tree))

;;; TODO rbt-delete
(defn rbt-delete [tree x]
  )

;;;; Testing fns ;;;;
(defn- rand-test
  ([constructor-fn] (rand-test constructor-fn 20))
  ([constructor-fn n]
    (let [max-elem (inc (* 2 n))
          [x & xs] (map (fn [_] (rand-int max-elem)) (range n))]
      (reduce insert (constructor-fn nil x nil) xs))))

(def test-avl (partial rand-test avl))
(def test-rbt (partial rand-test (partial rbt :black)))
