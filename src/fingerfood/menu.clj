(ns fingerfood.menu
    (:require [fingerfood.input :as input] 
              [clojure.core.match :refer [match]])
    (:import (jline.console ConsoleReader)
             (java.util.regex Pattern)))

(declare Command)

(defprotocol Menu
"A hierarchical view of a collection which can be browsed in a user-friendly way: display, selection, paging and 
 navigation between levels and within recent browsing history.

 All navigation commands except 'select yield a Menu, and 'select's return is up to the implementation.

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

    (^Command get-command [this]
        "Retrieves and formats user input. Yields a Command which provides the user
         selection or indicates an exit from the menu - see Command., or nil
         to end browsing immediately.")

    (select [this selection]
        "Apply user selection (side-effect .e.g display) of a menu item - which could open
         another menu. A selection is returned from (selection ^Command command), 
         and can be nil (i.e. the command was not a selection, see Command.")

    (nested? [this]
        "If not making use of nested menus, can default to false/nil.
         Yields true when this menu was created by a menu with a different
         backing collection, e.g. when expanding an element of the originating menu's 
         collection.")) 


(defprotocol Command
 "Switches exposed by a menu command, mapping to mutually exclusive scenarios.
  Only one should yield a non-nil value."
    (up? [cmd]
        "Yields n when the user wants to exit the current menu to return to the 
         n-th menu above, or nil.")
    
    (end? [cmd]
        "Logically true only if the user wants to exit the menu hierarchy
         entirely.")

    (page? [cmd]
        "Yields a number of pages forward or backward (negative) to navigate to 
         in the current menu, or nil.")

    (history-n? [cmd]
        "Yields the number of  menus forward or back (negative) to navigate 
         in the history.")

    (menu? [cmd]
        "Logically true only if the current menu is requested.")

    (edit? [cmd]
        "Yields a regular expression as a java.util.Pattern for filtering purposes, or nil.")

    (selection? [cmd]
        "Yields the user selection for the current menu if selecting, or nil. 
         Translates into (select menu)."))


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


(defn make-command [ & {:keys [up end page menu selection edit]}]
    (reify Command
        (up? [_] up)
        (end? [_] end)
        (page? [_] page) 
        (menu? [_] menu)
        (edit? [_] edit)
        (selection? [_] selection)))


(declare command-up prep-command exit-up at-target printn)

(defn cmd-vector [cmd]
    [(end? cmd), (menu? cmd), (up? cmd), (page? cmd), (edit? cmd), (selection? cmd)])

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


(defn at-target 
"Yields true if the command exited into the menu targeted by the user.
"
[^fingerfood.menu.Command cmd]
    (if-let [ cmd (prep-command cmd :parse-only)]
        (if (:selection cmd)
            (throw (IllegalStateException. 
                "Can't select from and exit a menu at once"))
            (zero? (.up? cmd))))) 


(defn- prep-command
"Matches a command to its expected scenario value: if exiting a menu
 a (possibly nil) command (end? or up? switches) is returned, or
 a {:selection selection} map. If (up? cmd) is truthy, the returned
 command has its up? property decremented.
 "
[^fingerfood.menu.Command cmd & [parse-only?]]
(if-not cmd 
    nil
    (match [ (.end? cmd), (.up? cmd), (.selection? cmd) ] 
        [  _  nil nil  ] nil ;; done
        [ nil  n  nil  ] (if (parse-only? cmd) (command-up n))
        [ nil nil pick ] {:selection pick})))


(defn command-up
"Yields a DefaultCommand with its up? switch set to (dec n)"
[ n ] 
    (make-command :up (dec n)))
