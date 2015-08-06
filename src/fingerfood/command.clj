(ns fingerfood.command)

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

(defn make-command [ & {:keys [up end page menu selection edit]}]
    (reify Command
        (up? [_] up)
        (end? [_] end)
        (page? [_] page) 
        (menu? [_] menu)
        (edit? [_] edit)
        (selection? [_] selection)))



(defn cmd-vector [cmd]
    [(end? cmd), (menu? cmd), (up? cmd), (page? cmd), (edit? cmd), (selection? cmd)])


#_(defn command-up
"Yields a Command with its up? switch set to (dec n)"
[ n ] 
    (make-command :up (dec n)))


#_(defn at-target 
"Yields true if the command exited into the menu targeted by the user.
"
[^fingerfood.menu.Command cmd]
    (if-let [ cmd (prep-command cmd :parse-only)]
        (if (:selection cmd)
            (throw (IllegalStateException. 
                "Can't select from and exit a menu at once"))
            (zero? (.up? cmd))))) 


#_(defn- prep-command
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


