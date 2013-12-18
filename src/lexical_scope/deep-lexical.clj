(ns lexical-scope.deep-lexical)


(def reshape-recur)

(defn expand-dispatcher
  [vars input]
  (if (seq? (first input))
    (first (first input))))

(defmulti expand
  expand-dispatcher)

(defmethod expand :default
  [vars input]
  (cons
   (reshape-recur vars (first input))
   (reshape-recur vars (rest input))))

(defmethod expand 'define
  [vars [[_ name value] & rst]]
  (let [sym (gensym),
        new-vars (assoc vars name sym)]
    (cons `(~'def ~sym (~'atom ~(reshape-recur vars value)))
          (reshape-recur new-vars rst))))

(defmethod expand 'setq
  [vars [[_ name value] & rst]]
  (cons `(~'reset! ~(get vars name) ~(reshape-recur vars value))
        (reshape-recur vars rst)))

(defn combine-pair-map
  [new-pair map-to-combine]
  {'pairs (concat new-pair (get map-to-combine 'pairs))
   'body (get map-to-combine 'body)})

(defn parse-pairs
  [vars pairs body]
  (if (= [] pairs)
    {'pairs [] 'body (reshape-recur vars body)}
    (let [left (first pairs),
          right (second pairs)]
      (let [new-vars
            (if (get vars left)
              (assoc vars left left)
              vars)]
        (combine-pair-map
         [left (reshape-recur vars right)]
         (parse-pairs new-vars (rest (rest pairs)) body))))))

(defmethod expand 'let
  [vars [[_ pairs & body] & rst]]
  (cons
   (let [ret (parse-pairs vars pairs body)]
     `(~'let ~(apply vector (get ret 'pairs)) ~@(get ret 'body)))
   (reshape-recur vars rst)))


(defn reshape-recur
  [vars input]
  (if (and
       (not= '() input)
       (seq? input))
    (expand vars input)
    (if-let [sub (get vars input)]
      (if (= sub input)
        input
        `(~'deref ~sub))
      input)))


(defmacro lexical-do
  [& body]
  `(do ~@(reshape-recur {} body)))
