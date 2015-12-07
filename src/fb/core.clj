(ns fb.core
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn modulate [n thing]
  "Given an integer n and a thing, returns a function taking a single integer
   argument a. The returned function will return thing if a mod n is zero;
   otherwise, it will return nil."
  (fn [a] (when (zero? (mod a n)) thing)))

(def remove-nil-elements (map #(filterv some? %)))

(defn spreading
  "Given a collection of functions fs as an argument, returns a mapping transducer
   that transforms an element into a vector with the element in the first position,
   and any non-nil results of succeeding functions as the remaining values."
  [& fs] (comp (map (apply juxt identity fs)) remove-nil-elements))

(def named?
  "Tests whether its argument is one of the default types that can be converted to
   a string using clojure.core.name."
  (some-fn keyword? symbol? string?))

(defn maybe-name [thing]
  "Attempts to convert thing to a string representing its name; otherwise, returns
   thing unchanged."
  (if (named? thing) (name thing) thing))

(defn squash [[i & tags]]
  "Given a vector, if any of the elements (tags) after the first are non-nil, will
   return a flattened string representation of all of them; otherwise, returns the
   first element."
  (if (some some? tags)
    (apply str (map maybe-name tags))
    i))

(def squashing (map squash))

(def fizz? (modulate 3 :fizz))

(def buzz? (modulate 5 :buzz))

(def pop? (modulate 4 :pop))

(def fizzbuzzifying (spreading fizz? buzz?))

(def fizzbuzzpopifying (spreading fizz? buzz? pop?))

(def fizzbuzzing (comp fizzbuzzifying squashing))

(def fizzbuzzpopping (comp fizzbuzzpopifying squashing))

(def infinite-fizzbuzz (sequence fizzbuzzing (drop 1 (range))))

(def short-fizzbuzz (take 100 infinite-fizzbuzz))

(defn prettify-noise [[i :as noise]]
  (format "%3s: %s\n" i (squash noise)))

(defn mapify [[i & tags]] (when tags {i (vec tags)}))

(def defizzing (filter (fn [v] (not-any? #(= % :fizz) v))))


;;; Let's test some properties
(defn positive-int-gen* [& [max]]
  (gen/large-integer* {:min 1 :max (or max Long/MAX_VALUE)}))

(def positive-int-gen
  (positive-int-gen*))

;; We're only going to consider positive integers to be fizzbuzzable, so
;; let's make it easy to generate a bunch of them.
(defn positive-ints-gen* [& [max]]
  (gen/vector (positive-int-gen* max)))

(def positive-ints-gen (positive-ints-gen*))

;; To make things easy, we'll abstract out the concept of applying a transducer to
;; a generator. The generator g will need to produce output that's appropriate to
;; pass through a transducer (e.g., a collection). We won't generalize this
;; fully, and will just produce output as a sequence
;; (defn trans-gen [xf g]
;;   (gen/let [v g]
;;     (sequence xf v)))


;; Here's another version that's more combinator-y. It'll work on collections (as
;; long as empty can return an empty version of a collection of that type), and
;; on single values (which it wraps to feed through the transducer and then unwraps)
(defn trans-gen [xf g]
  (gen/let [thing g]
    (if-let [coll (empty thing)]
      (into coll xf thing)
      (first (into [] xf [thing])))))

;; Here it is: a generator that spits out a sequence of fizzbuzzed numbers
(def fizzbuzz-gen (partial trans-gen fizzbuzzing))

;; For fun, here's another so we can also give fizzbuzzpop a workout
(def fizzbuzzpop-gen (partial trans-gen fizzbuzzpopping))

;; We'll keep building up small, general pieces. This one defines a property where,
;; if a generator outputs something that's a number, that number cannot be evenly
;; divisible by a chosen integer n.
(defn no-divisibles [n g]
  (prop/for-all [v g]
    (not (some #(and
                 (number? %)
                 (= (mod % n) 0))
               v))))

;; Since n is the first argument of no-divisibles, it's easy to build some specific,
;; named properties that accept a single generator as their argument
(def no-threes (partial no-divisibles 3))
(def no-fours (partial no-divisibles 4))
(def no-fives (partial no-divisibles 5))

(comment
  (tc/quick-check 100 (no-threes (fizzbuzz-gen positive-ints-gen)))
  (tc/quick-check 100 (no-fives (fizzbuzz-gen positive-ints-gen)))

  ;; This one will probably fail!
  (tc/quick-check 100 (no-fours (fizzbuzz-gen positive-ints-gen)))

  ;; But not this one:
  (tc/quick-check 100 (no-fours (fizzbuzzpop-gen positive-ints-gen)))
  )

;; Now, let's give the fizzbuzzed numbers a workout. To do that, let's generate
;; only numbers that will definitely be subjected to a conversion.
;; We make sure the generated integers aren't too large to prevent an overflow.
;; Thus protected, we simply mutiply by n to get our multiples.
;; Another option would be to use gen/such-that, but this avoids retries.
(defn multiple-of-gen [n]
  (gen/let [i (positive-int-gen* (/ Long/MAX_VALUE n))]
    (* i n)))

(defn multiples-of-gen [n]
  (gen/vector (multiple-of-gen n)))

(def multiples-of-three   (multiples-of-gen 3))
(def multiples-of-four    (multiples-of-gen 4))
(def multiples-of-five    (multiples-of-gen 5))
(def multiples-of-fifteen (multiples-of-gen 15))

(defn fizzed? [x]
  (re-find #"^fizz" x))
(defn buzzed? [x]
  (re-find #"buzz$" x))

(defn all-fizzed-prop [xf]
  (prop/for-all [v (trans-gen xf multiples-of-three)]
    (every? #(re-find #"^fizz" %) v)))

(defn all-buzzed-prop [xf]
  (prop/for-all [v (trans-gen xf multiples-of-five)]
    (every? #(re-find #"buzz$" %) v)))

(defn all-fizzbuzzed-prop [xf]
  (prop/for-all [v (trans-gen xf multiples-of-fifteen)]
    (every? #(= "fizzbuzz" %) v)))

;; (defn not-multiple-of-gen [ns g]
;;   (let [preds (apply juxt (map (fn [n] #(not= (mod % n) 0)) ns))
;;         pred  #(some identity (preds %))]
;;     (gen/such-that pred g 100)))

(defn not-multiple-of-gen [n g]
  (gen/such-that #(not= (mod % n) 0) g 100))

(defn not-multiple-of-three [g]
  (not-multiple-of-gen 3 g))

(defn not-multiple-of-five [g]
  (not-multiple-of-gen 5 g))

(defn not-multiple-of-fifteen [g]
  (not-multiple-of-gen 15 g))

(defn not-fizzable-or-buzzable [g]
  (->> g (not-multiple-of-gen 3) (not-multiple-of-gen 5)))

(def multiples-of-three-not-five
  (->> (multiple-of-gen 3) (not-multiple-of-gen 5) gen/vector))

(def multiples-of-five-not-three
  (->> (multiple-of-gen 5) (not-multiple-of-gen 3) gen/vector))

(defn threes-fizz-prop [xf]
  (prop/for-all
      [v (->> (multiple-of-gen 3)
              (not-multiple-of-gen 5)
              (trans-gen xf))]
    (every? #(= % "fizz") v)))

(defn fives-buzz-prop [xf]
  (prop/for-all [v (trans-gen xf multiples-of-five-not-three)]
    (every? #(= % "buzz") v)))

(defn leftovers-unchanged-prop [xf]
  (prop/for-all [v (->> positive-int-gen not-fizzable-or-buzzable gen/vector)]
    (let [w (into [] xf v)]
      (= v w))))

(comment

  ;; We now have excellent coverage of all the cases
  (tc/quick-check 100 (threes-fizz-prop fizzbuzzing))
  (tc/quick-check 100 (fives-buzz-prop fizzbuzzing))
  (tc/quick-check 100 (all-fizzbuzzed-prop fizzbuzzing))
  (tc/quick-check 100 (leftovers-unchanged-prop fizzbuzzing))

  ;; These properties do not hold for fizzbuzzpop
  (tc/quick-check 100 (threes-fizz-prop fizzbuzzpopping))
  (tc/quick-check 100 (prop/for-all [x (->> (multiple-of-gen 3)
                                            (not-multiple-of-gen 5)
                                            (trans-gen fizzbuzzing))]
                        (= x "fizz")))

  (Tc/quick-check 100 (fives-buzz-prop fizzbuzzpopping))
  (tc/quick-check 100 (all-fizzbuzzed-prop fizzbuzzpopping))
  (tc/quick-check 100 (leftovers-unchanged-prop fizzbuzzpopping))

  ;; Some properties can be used to test both
  (tc/quick-check 100 (all-fizzed-prop fizzbuzzing))
  (tc/quick-check 100 (all-fizzed-prop fizzbuzzpopping))
  (tc/quick-check 100 (all-buzzed-prop fizzbuzzing))
  (tc/quick-check 100 (all-buzzed-prop fizzbuzzpopping))

  )
