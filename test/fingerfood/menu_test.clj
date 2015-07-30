(ns fingerfood.menu-test
    (:require [fingerfood.menu :refer :all])
    (:require [clojure.test :refer :all]))

(deftest make-command-syntax-test
    (testing "CommandSyntax factory fn"
       (let [ re #"\d*[uU]" error-fn (fn [_ _ _] "Error")
              cs (make-command-syntax re error-fn) ] 
            (is (valid?- cs "2"))
            (is (valid? cs "22U"))
            (is (not (valid?- cs "x")))
            (is (not (valid? cs "uWHAT?")))
            (is (= "Error" (error-fn cs re ""))))))
    
