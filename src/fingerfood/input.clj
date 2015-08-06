(ns fingerfood.input
    (:require [fingerfood.command :as cmd])
    (:require [clojure.core.match :refer [match] :as m])
    (:import (jline.console ConsoleReader)
             (java.util.regex Pattern)))
    
(defprotocol Syntax
"Validates a command input, both partial and complete. The intent is to allow 
 an input processor to yield a valid command or fail, as soon as possible."
    (valid?- [this input]
        "Yields a truthy value only if (valid? this input) is truthy, or 
         no syntax error has been introduced for an incomplete input")

    (valid? [this input]
        "Yields a truthy value only if input results in a valid command.")

    (^String error [this input]
        "Generates a string describing a syntax error"))


(defprotocol InputDialog
"Retrieves and validates user input, and yields a parseable command, or nil.
"
    (get-cmd [this]
        "Gathers input to generate the next command."))


(defprotocol ReadChar
"Reads input one character at a time.
" 
    (^int read-char [this]
        "Yields the next read char as it is natively read"))

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
(def ^:dynamic *command-re* #"(\d*)([ mMpPnNuUqQ])?|:e (.*)|")
(def ^:dynamic *command-re-partials* [ #"\d*", #":(e|e |e .*)?"])


(defn make-syntax 
"Yields a Syntax instance backed by complete-re and partial-res as 
 complete and partial command sytaxes, resp.
"
([complete-re partial-res error-fn]
    (reify Syntax
        (valid?- [_ s]
            (->> partial-res
                 (some #(re-matches % s))))
        (valid? [_ s]
            (re-matches complete-re s))
        (error [this s]
            (error-fn [[complete-re partial-res]] s))))
([error-fn]
    (make-syntax *command-re* *command-re-partials* error-fn))
([]
    (make-syntax (fn [_] "Error, ppPAL!!"))))


(defn make-character-reader
"Yields an instance ReadChar impl using jline.ConsoleReader.
"
[]
(let [term (ConsoleReader.)]
    (reify ReadChar
        (read-char [_] (.readCharacter term)))))


(defn- parse-int 
([s zero?]
    (match [s (empty? s) zero?]
        [ _ true true  ] 0
        [ _ true false ] nil
        [ s   _   _    ] (Integer/parseInt s)))
([s]
    (parse-int s true)))
        

(defn parse-command 
"Parses a validated command input into a Command object, using
 *command-re* as syntax.
"
[^String cmd]
(let [ mc (fn ([type] (cmd/make-command type true))  
              ([type digs & [re]] (cmd/make-command type 
                                    (if re (Pattern/compile re)
                                            (parse-int digs))))) ]
    (match (re-matches *command-re* cmd)
   ;; full, digits, cmd-type, re-str 
        [_   _  (:or "q" "Q")   _  ]   (mc :end)
        [_   _  (:or "m" "M")   _  ]   (mc :menu)
        [""  _  _               _  ]   (mc :up "1")
        [_   d  (:or "u" "U")   _  ]   (mc :up d)
        [_  ""  (:or " " "n" "N") _]   (mc :page "1")
        [_   d  (:or " " "n" "N") _]   (mc :page d)
        [_  ""  (:or "p" "P")   _  ]   (mc :page "-1")
        [_   d  (:or "p" "P")   _  ]   (mc :page (str "-" d))
        [_   nil    nil         re ]   (mc :edit nil re)
        [_   d      nil       nil  ]   (mc :selection d))))


(defn as-str [chars]
    (apply str chars))


(defn enter-key? [ c ]
    (re-matches #"[\r\n]" (str c)))


(defn printn [& args]
    (apply print args) (flush))


(defn get-cmd-impl [^fingerfood.input.Syntax stx ^fingerfood.input.ReadChar rdr ^String prompt]
    (printn prompt)
    (let [ read #(char (.read-char rdr))
           try-again #(do (println (.error stx (as-str %)))
                        (printn prompt))

           ;;when testing this prevents seeing a CR overwrite by the result
           return (fn [res] (println) res)]
        (loop [c (read) chars []]
            (let [ 
                   ;;  necessary in repl-mode at least  
                   _ (printn c) 

                   chars+ (if (= \return c) chars (conj chars c))
                   cmd (as-str chars+)
                   #_(printn (str "CHAR: >" c "<, CHARS+:>" chars+ "<, CMD:>" cmd "<\n"))]

            (cond 
                (enter-key? c) (if (.valid? stx cmd) (return cmd)
                                   (do (try-again chars+)
                                        (recur (read) [])))
                (.valid?- stx cmd) (recur (read) chars+)
                (.valid? stx cmd) (return cmd) 
                :else
                    (do (try-again chars+)
                        (recur (read) [])))))))

(defn make-input-dialog 
([prompt re re-partials error-fn] 
    (let [ syntax (make-syntax re re-partials error-fn)
           reader  (make-character-reader)]
        (reify InputDialog
            (get-cmd [this]
                (get-cmd-impl syntax reader prompt)))))
([]
    (make-input-dialog 
            *default-menu-prompt* 
            *command-re* 
            *command-re-partials* 
            (fn [_ s] 
                (str "Error: " s " doesn't follow syntax <" *command-re* ">"))))) 
