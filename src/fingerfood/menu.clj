(ns fingerfood.menu
    (:require [fingerfood.input :as input] 
              [clojure.core.match :refer [match]])
    (:import (jline.console ConsoleReader)
             (java.util.regex Pattern)))

(defprotocol Menu
"A hierarchical view of a collection which can be browsed in a user-friendly way: display, selection, paging and 
 navigation between levels and within recent browsing history.

 All navigation commands except 'select must yield a Menu.

 For all menu commands, a request that can't be accomdated e.g. for an out-of-bounds target, should result in a
 sensible outcome: an error message and repeat prompt, or cycling are preferred rather than nil or throwing 
 an exception.
"
    (coll [this]
        "Yields the backing collection")

    (pagelen [this] 
        "Provides a measure of the maximum length/size of the backing 
         collection that can be displayed.")
         
    (cursor [this] 
        "Used to access the first item in the backing collection")

    (display [this]
        "Displays the menu.")

    (page [this n ]
        "Yields a new menu of items from the backing list, going forward (backward 
         if minus) by n pages")

    (history [this n]
         "Akin to a web browser's Forward/back buttons. Yields the nth menu (minus if going back)
         in the history, from the current menu.")

    #_(^Command get-command [this]
        "Retrieves and formats user input. Yields a Command which provides the user
         selection or indicates an exit from the menu - see Command., or nil
         to end browsing immediately.")

    (select [this selection]
        "Applies user selection (side-effect .e.g display) of a menu item and yields
         the result - either an item or another menu.")

    (nested? [this]
        "If not making use of nested menus, can default to false/nil.
         Yields true when this menu was created by a menu with a different
         backing collection, e.g. when expanding an element of the originating menu's 
         collection.")) 


(defn make-menu 
"Yields a menu hierarchy backed by data. Each menu is a sub sequence of pagelen
 from data, and menus at the same level don't overlap. Selecting an item 
 on a menu causes (select-fn item) to be called the first time, and the resulting
 value to be used on remaining selects to the same item.
 
For example the following interactions and hierarchies, where ^ points at the menu cursor,
i.e. the menu ready to be manipulated.
=> (def m (make-menu [1 2 3 4 5] 2 (fn [i] (range n)))
=> [[1 2] [3 4] [5]]
;;    ^ 
=> (display menu)
=> ;;display specific of items 1, 2
=> (select menu 2)
=> [0 1]
 "
[data pagelen select-fn])



(defn vector-menu 
"Creates the starting menu of pagelen items from items; items is a vector."
[items, paglen]
#_(let [ pages (partition-all pagelen items)]))



#_(defn browse 
"Coordinates menu interactions with the user using semantics described in 
 the Menu protocol. Defines a session for all menus with the same backing
 collection.
"
[^fingerfood.menu.Menu menu]
  (display menu)
  (loop [menu menu]
    (when-let [ cmd (-> (get-command menu) prep-command) ]
        #_(match [ (cmd-vector cmd) ]
    ;; end, menu, up, page, edit, selection
        [_   nil  nil  nil  nil    nil   ]  nil
        [nil  _   nil  nil  nil    nil   ]  (display menu)
        [nil nil   n   nil  nil    nil   ]  (up menu n)
        [nil nil  nil   n   nil    nil   ]  (page menu n)
        [nil nil  nil  nil   re    nil   ]  (edit menu re)
        [nil nil  nil  nil  nil    sel   ]  (select menu sel))

        (if-let [ pick (:selection cmd) ] 
            (let [new-menu (select menu pick)]
                (if (nested? new-menu)

                    ;; Enter new browsing session
                    ;; and return here or higher when done

                    (when-let [ exit (browse new-menu) ] 
                        (if (at-target exit) 
                            (do 
                                (display menu)           
                                (recur menu))
                            (exit-up exit)))

                    ;; new menu is in current session
                    (recur new-menu)))

         ;; End session
         cmd))))
