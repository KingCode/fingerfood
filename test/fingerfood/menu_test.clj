(ns fingerfood.menu-test
    (:require [fingerfood.menu :refer :all])
    (:require [clojure.test :refer :all]))

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
    
