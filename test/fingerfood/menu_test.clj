(ns fingerfood.menu-test
    (:require [fingerfood.menu :refer :all])
    (:require [clojure.test :refer :all])
    (:require [clojure.core.match :refer [match] :as m]))

;; The referred RE:
;;    *command-re* #"(\d*[mMpPnNuUqQ]?|:e .*)|")
;;

(deftest make-command-syntax-test
    (testing "default command syntax validation"
       (let [ re *command-re* 
                parts *command-re-partials* ;; [#"\d*"] 
                error-fn (fn [_ _ _] "Error")
                syn (make-command-syntax re parts error-fn) ] 
            (is (valid?- syn "2"))
            (is (valid? syn "22U"))
            (is (not (valid?- syn "22U")))
            (is (valid? syn "123"))
            (is (valid? syn ""))
            (is (valid? syn "m"))
            (is (valid? syn " "))
            (is (valid?- syn ":"))
            (is (valid?- syn ":e #\"yo!\""))
            (is (valid? syn ":e #\"yo!\""))
            (is (not (valid?- syn "x")))
            (is (not (valid? syn "uWHAT?")))
            (is (= "Error" (error-fn syn re ""))))))
    
(defrecord CommandImpl [end menu up page edit select]
    Command
        (end? [_] end)
        (menu? [_] menu)
        (up? [_] up)
        (page? [_] page)
        (edit? [_] edit)
        (selection? [_] select))
        
(defn as-map [cmd]
    {:end (end? cmd), :menu (menu? cmd), :up (up? cmd), 
      :page (page? cmd), :edit (edit? cmd), :selection (selection? cmd)})

(defn exclusive?+kv 
"Yields the command's [type value] if exclusive, or nil
"
[cmd]
(->> (as-map cmd) 
    (filter (fn [[k v]] (when v [k v])))
    (#(when (= 1 (count %)) 
        (first %)))))


(deftest exclusive-type-test-meta
    (testing "command exclusivity test method is itself correct"
        (let [cmd1 (->CommandImpl nil :menu nil nil nil nil)
              cmd2 (->CommandImpl nil nil nil nil nil 2)
              cmd3 (->CommandImpl nil nil nil nil ":e yup" nil)
              cmd4 (->CommandImpl nil :menu nil nil ":e yup" nil)
              cmd5 (->CommandImpl nil nil nil nil nil nil) ]
            (is (= [:menu :menu] (exclusive?+kv cmd1)))
            (is (= [:selection 2] (exclusive?+kv cmd2)))
            (is (= [:edit ":e yup"] (exclusive?+kv cmd3)))
            (is (nil? (exclusive?+kv cmd4)))
            (is (nil? (exclusive?+kv cmd5)))))) 

(defn present? [[exp-k _] [act-k act-v]]
   (and (not (nil? act-v)) (= exp-k act-k))) 

(defn pat= [ re1 re2 ]
    (= (.toString re1) (.toString re2)))

(deftest parse-command-test
    (testing "a valid user input parses into the correct Command"
        (let [ get #(exclusive?+kv (parse-command %))
               i1 "", act1 (get i1)      exp1 [:up :up]
               i2 "2", act2 (get i2)     exp2 [:selection 2] 
               i3 "2p", act3 (get i3)    exp3 [:page -2] 
               i4 "2P", act4 (get i4)    exp4 [:page -2]
               i5 "p",  act5 (get i5)    exp5  [:page -1]
               i6 "P", act6 (get i6)     exp6  [:page -1]
               i7 "10n", act7 (get i7)   exp7 [:page 10]
               i71 "n", act71 (get i71)  exp71 [:page 1]
               i8 "m",  act8 (get i8),   exp8 [:menu :menu]
               i9 "2M", act9 (get i9)    exp9 [:menu :menu]
               i10 ":e ", act10 (get i10),  exp10 [:edit #""]
               i11 ":e mew", act11 (get i11), exp11 [:edit #"mew"]]
            (is (present? exp1 act1))
            (is (= exp2 act2))
            (is (= exp3 act3))
            (is (= exp4 act4))
            (is (= exp5 act5))
            (is (= exp6 act6))
            (is (= exp7 act7))
            (is (= exp71 act71))
            (is (present? exp8 act8))
            (is (present? exp9  act9))
            (is (pat= exp10 act10))
            (is (pat= exp11 act11)))))
