(ns fingerfood.menu-test
    (:require [fingerfood.menu :refer :all])
    (:require [clojure.test :refer :all])
    (:require [clojure.core.match :refer [match] :as m]))

(deftest make-menu-test
    (testing "Given a page size and a sequential, creates a menu hierarchy"))
