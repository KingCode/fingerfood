(ns fingerfood.menu
    (:require [clojure.core.match :refer [match] :as m])
    (:import [jline.console ConsoleReader]))

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
    (up-n? [cmd]
        "Yields n when the user wants to exit the current menu to return to the 
         n-th menu above, or nil.")
    
    (end? [cmd]
        "Yields a truthy value when the user wants to exit the menu hierarchy
         entirely, nil otherwise.")

    (page-n? [cmd]
        "Yields a number of pages forward or backward (negative) to navigate to 
         in the current menu, or nil. Translates into Menu's next-n-pages/prev-n-pages")

    (history-n? [cmd]
        "Unused - see Menu protocol's forward/back methods. Yields the number of
         menus forward or back (negative) to navigate away from this menu in the
         history, maps to Menu's forward/back methods.")

    (menu? [cmd]
        "Yields a truthy value when the current menu is requested; nil/false otherwise.
         Translates into Menu's display method.")

    (selection? [cmd]
        "Yields the user selection for the current menu if selecting, or nil. 
         Translates into (select menu)."))

        
(defprotocol CommandSyntax
"Validates a command input, both partial and complete. The intent is to allow 
 an input processor to yield a valid command or fail, as soon as possible."
    (valid?- [this input]
        "Yields a truthy value only if (valid? this input) is truthy, or 
         no syntax error has been introduced for an incomplete input")

    (valid? [this input]
        "Yields a truthy value only if input results in a valid command.")

    (^String error [this input]
        "Generates a string describing a syntax error"))


(defprotocol CommandParser
"Parses a command-string into a Command instance.
"
    (^ifa.browser.Command parse [^String s]))


(defprotocol InputDialog
"Retrieves and validates user input, and yields a parseable command, or nil.
"
    (get-cmd [this]
        "Retrieves enough input to generate the next command."))


(defprotocol ReadChar
"Reads input one character at a time.
" 
    (^int read-char [this]
        "Yields the next read char as it is natively read"))

    
(defn make-command-syntax [complete-re partial-res error-fn]
    (reify CommandSyntax
        (valid?- [_ s]
            (->> partial-res
                 (some #(re-matches % s))))
        (valid? [_ s]
            (re-matches complete-re s))
        (error [this s]
            (error-fn [complete-re partial-res] s))))
        

(defn make-character-reader
"Yields an instance ReadChar impl using jline.ConsoleReader.
"
[]
(let [term (ConsoleReader.)]
    (reify ReadChar
        (read-char [_] (.readCharacter term)))))


(defn make-command [ & {:keys [up end page menu selection]}]
    (reify Command
        (up-n? [_] up)
        (end? [_] end)
        (page-n? [_] page) 
        (menu? [_] menu)
        (selection? [_] selection)))


(defn- parse-int [s zero?]
    (match [s (empty? s) zero?]
        [ _ true true  ] 0
        [ _ true false ] nil
        [ s   _   _    ] (Integer/parseInt s)))
        

(defn parse-command 
"Parses a validated command input into a Command object, using
 *command-re* as syntax.
"
[^String cmd]
   (condp re-matches cmd
        #".*[mM]$" (make-command :menu true)
        #".*[qQ]$" (make-command :end true)
        #"^(\d*)[uU]$" :>> #(make-command :up (parse-int (nth % 1)))
        #""           :>> (make-command :up 1)
        #"^(\d*)[nN]$" :>> #(make-command :page (parse-int (nth % 1)))
        #"^(\d*)[pP]$" :>> #(make-command :page (- (parse-int (nth % 1))))
        #"^\d+$" :>> #(make-command :selection (parse-int %))))


(declare command-up prep-command exit-up at-target printn)

(defn browse 
"Coordinates menu interactions with the user using semantics described in 
 the Menu protocol. Defines a session for all menus with the same backing
 collection.
"
[^fingerfood.menu.Menu menu]
  (display menu)
  (loop [menu menu]
    (when-let [ cmd (-> (get-command menu) prep-command) ]
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


(defn menu-prompt 
"Prompt template into which are inserted  selection and filtering tags"
([select-tag filter-tag]
    (let [  select-tag (if select-tag (str "[" select-tag "] ") "")
            filter-tag (if filter-tag (str "[" filter-tag "] ") "")]
      (str select-tag filter-tag
        "[Menu] [[n] Previous] [[n] Next] [[n] Up} [Quit] \n(<Enter> to exit this menu):")))
([select-tag] 
    (menu-prompt select-tag nil)))


(def ^:dynamic *default-menu-prompt* (menu-prompt "Number"))

(defn as-str [chars]
    (apply str chars))

(def ^:dynamic *command-re* #"(\d*[ mMpPnNuUqQ]?|:e .*)|")
(def ^:dynamic *command-re-partials* [ #"\d*", #":(e|e |e .*)?"])

#_(defn read-command
"Reads characters from r and yields the command string as soon as 
 determined, or until newline is read.
 "
([term syntax-re]
(loop [ i (.readCharacter term) chars []]
    (let [c (char i)]
        (if (= \newline c) 
           (as-str chars)
           (let [ chars+ (conj chars c)
                  cmd (as-str chars+) ]
                (if
                    (syntax-ok? cmd) cmd
                    (recur (.readCharacter term) chars+))))))) 
([]
    (read-command (ConsoleReader.))))
                

(defn get-cmd-impl [^fingerfood.menu.CommandSyntax stx ^fingerfood.menu.ReadChar rdr ^String prompt]
    (printn prompt)
    (let [ read #(char (.readChar rdr))
           try-again #(do (println (.error stx (as-str %)))
                        (printn prompt))]
        (loop [c (read) chars []]
            (let [ chars+ (conj chars c)
                   cmd (as-str chars+)]
            (cond 
                (= \newline c) (if (.valid? stx cmd) cmd
                                   (do (try-again chars+)
                                        (recur (read) chars+)))
                (.valid?- cmd) (recur (read) chars+)
                (.valid? cmd) cmd 
                :else
                    (do (try-again chars+)
                        (recur (read) chars+)))))))
    
(defn make-input-dialog [prompt re error-fn] 
    (let [ syntax (make-command-syntax re error-fn)
           reader  (make-character-reader)]
        (reify InputDialog
            (get-cmd [this]
                (get-cmd-impl syntax reader prompt)))))


(defn printn [& args]
    (apply print args) (flush))
