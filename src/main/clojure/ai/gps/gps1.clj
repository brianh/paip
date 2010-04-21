;;;; GPS from PAIP
;;;;
;;;; This is as close a port as possible (other than using sets).
;;;;

(ns ai.gps
  (:require [clojure.set :as set]))

(comment
  (load-file "/home/brian/code/clj/paip/src/main/clojure/ai/gps/gps1.clj")
)

(declare apply-op)

(def *state* nil)
(def *ops* nil)

(defn make-op [action pre-conds alist dlist]
  {:action action :preconds pre-conds :add-list alist :del-list dlist})

(defn appropriate? [goal]
  (fn [op]
    (contains? (:add-list op) goal)))

(defn achieved? [goal]
  "A goal is achieved if it already holds, or if there is an appropriate op
   for it that is applicable."
  (or (contains? *state* goal)
      (some apply-op (filter (appropriate? goal) *ops*))))

(defn achieved-all? [goals]
  "Try to achieve each goal, then make sure they still hold."
  (and (every? achieved? goals) (every? identity (map *state* goals))))

(defn GPS [state goals ops]
  "General problem solver: achieve all goals using 'ops'."
  (binding [*state* state
	    *ops* ops]
    (if (achieved-all? goals) :solved)))

(defn apply-op [op]
  "Print a message and update *state* if op is applicable."
  (when (achieved-all? (:preconds op))
    (prn (list ':executing (:action op)))
    (set! *state* (set/difference *state* (:del-list op)))
    (set! *state* (set/union *state* (:add-list op)))))

(def *school-ops* [
		   (make-op :drive-son-to-school #{:son-at-home :car-works} #{:son-at-school} #{:son-at-home})
		   (make-op :shop-installs-battery #{:car-needs-battery :shop-knows-problem :shop-has-money} #{:car-works} #{})
		   (make-op :tell-shop-problem #{:in-communication-w-shop} #{:shop-knows-problem} #{})
		   (make-op :telephone-shop #{:know-phone-number} #{:in-communication-w-shop} #{})
		   (make-op :lookup-shop-number #{:have-phone-book} #{:know-phone-number} #{})
		   (make-op :give-shop-money #{:shop-knows-problem :have-money} #{:shop-has-money} #{:have-money})])

(comment
  (GPS #{:son-at-home :car-works} #{:son-at-school} *school-ops*)
  (GPS #{:son-at-home :car-needs-battery :have-money :have-phone-book} #{:son-at-school} *school-ops*)
  (GPS #{:son-at-home :car-needs-battery :have-money :have-phone-book} #{:have-money :son-at-school} *school-ops*)
)