

;; DEPS

(local char_modifier (require :core.keymap.char_modifier))
(local _16n (require :pidgins.lib._16n))
(local inspect (require :pidgins.lib.inspect))

;; (local s (require :sequins))

;; works -> I assume it evals the Lua
(local f (require :pidgins.lib.fennel.fennel))
;; does not -> I assume it evals the Fennel
;; (local f (require :fennel))

(local clj (require :pidgins.lib.cljlib))
(import-macros cljm :pidgins.lib.cljlib)

(import-macros {: match-val-syms} :pidgins.lib.macros)



;; CONSTS

(local screen-w 128)
(local screen-h 64)

(local chan-btn :btn)
(local chan-enc :enc)
(local chan-grid :grid)
(local chan-hid :hid)
(local chan-hid-keyboard :hid/keyb)
(local chan-midi :midi)
(local chan-midi-16n :midi/16n)



;; CONF

(var rules {})



;; STATE

(var g nil)



;; CORE

;; TODO: fix those to honor maps
(fn pprint [v]
  (print (inspect v)))

(fn first [tbl]
  (. tbl 1))

(fn last [tbl]
  (. tbl (length tbl)))

(fn rest [tbl]
  [((or table.unpack _G.unpack) tbl 2)])



;; CORE - MAPS

(fn flatten [t level]
  "Take nested map t and flatten the keys to be on a single level."
  (let [out {}
        level (or level "")]
    (each [k v (pairs t)]
      (if (= (type v) "table")
          ;; at branch
          (let [flat-v (flatten v "  ")]
            (each [k2 v2 (pairs flat-v)]
              (let [k-path {}]
                (each [_ k2-el (pairs (clj.reverse k2))]
                  (table.insert k-path k2-el))
                (table.insert k-path k)
                (tset out (clj.reverse k-path) v2))))
          ;; at leaf
          (tset out [k] v)))
    out))

(fn get-path [map k-path]
  "Basically (apply . map k-path)."
  (var v-tmp map)
  (each [_ k (pairs k-path)]
    (set v-tmp (?. v-tmp k)))
  v-tmp)

(fn set-path [map k-path v]
  "Set value in map at k-path to v (mutates)."
  (var v-tmp map)
  (let [l (length k-path)]
    (each [i k (ipairs k-path)]
      (if (= i l)
          (tset v-tmp k v)
          (set v-tmp (?. v-tmp k))))))

;; REVIEW: just use `cljm.into'?
(fn m/+ [map k-v]
  (let [flat (flatten k-v)]
    (each [k-path v (pairs flat)]
      (set-path map k-path (+ (get-path map k-path)
                              v)))))
(fn m/- [map k-v]
  (let [flat (flatten k-v)]
    (each [k-path v (pairs flat)]
      (set-path map k-path (- (get-path map k-path)
                              v)))))



;; CORE - MATCH

(fn matches? [map map-matcher]
  "Basically a reimplem of the `match' macro as a fn.
Allows for better composability and having the matchers as a conf."
  (var res true)
  (let [flat-matcher (flatten map-matcher)]
    (each [k-path match-v (pairs flat-matcher)]
      (let [v (get-path map k-path)]
        (when (or (= v nil)
                  (not (= v match-v)))
          (set res false)))))
  res)



;; ENRICHMENT / NORMALIZATION

(fn btn->pgn [id state]
  {:chan chan-btn
   :msg {:i id
         :state state}})

(fn enc->pgn [id delta]
  {:chan chan-enc
   :msg {:id id
         :delta delta}})

(fn grid->pgn [device x y z]
  {:chan chan-grid
   :msg {:dev device
         :x x
         :y y
         :z z}})

(fn midi->pgn [device d]
  (let [msg (midi.to_msg d)]
    {:chan chan-midi
     :dev device
     :msg msg}))

(fn _16n->pgn [device d]
  (let [msg (midi.to_msg d)]
    {:chan chan-midi-16n
     :dev device
     :msg msg
     :slider (_16n.cc_2_slider_id msg.cc)}))

(fn hid->pgn [d]
  {:chan chan-hid
   :msg d})

(fn kbd-selected-map []
  (?. keyboard.keymap keyboard.selected_map))

(fn kbd-current-modifiers []
  (let [c-mods char_modifier.NONE
        c-mods (if (keyboard.shift) (bor c-mods char_modifier.SHIFT) c-mods)
        c-mods (if (keyboard.altgr) (bor c-mods char_modifier.ALTGR) c-mods)]
    c-mods))

(fn kbd-keycode->char [keycode]
  (. (kbd-selected-map) (kbd-current-modifiers) keycode))

(fn kbd->pgn [keycode v]
  {:chan chan-hid-keyboard
   :msg {:code keycode
         :v v
         :state (> v 0)
         :char (kbd-keycode->char keycode)}})



;; DSL

(fn matched-rule [pgn]
  (clj.some #(let [[cond _cb] $1]
               (when (matches? pgn cond)
                 $1))
            rules))

(fn process-pgn [pgn]
  (cljm.when-let [rule (matched-rule pgn)]
                 (let [[_ cb] rule]
                   (cb pgn))))

(fn process-pgn-hard-code [pgn]
  ;; (match pgn rules)
  (match pgn
    {:chan :midi
     :dev {:name "bleached"}
     :msg {:type "cc"}} (pprint pgn)

     {:chan :hid/keyb} (pprint pgn)

     {:chan :btn} (pprint pgn)
     {:chan :enc} (pprint pgn)

     {:chan :grid} (pprint pgn)))



;; LISTENER FNs

(fn cb-btn [id state]
  (let [pgn (btn->pgn id state)]
    (process-pgn pgn)))

(fn cb-enc [id delta]
  (let [pgn (enc->pgn id delta)]
    (process-pgn pgn)))

(fn make-cb-grid [device]
  (fn [x y z]
    (let [pgn (grid->pgn device x y z)]
      ;; (pprint pgn)
      (process-pgn pgn))))

(fn init-grid []
  (let [device {:id 1}]
    (set g (grid.connect))
    (set g.key (make-cb-grid device))))

(fn make-cb-midi [device]
  (fn [d]
    (let [pgn (midi->pgn device d)]
      (process-pgn pgn))))

(fn init-midi-all []
  (each [_ d (pairs midi.devices)]
    (when (and (~= d.port nil)
               (~= d.name "16n"))
      (let [device {:name d.name
                    :port d.port}
            m (midi.connect d.port)
            cb (make-cb-midi device)]
        (set m.event cb)))))

(fn make-cb-16n [device]
  (fn [d]
    (let [pgn (_16n->pgn device d)]
      ;; (pprint pgn)
      (process-pgn pgn))))

(fn init-midi-16n []
  (each [_ d (pairs midi.devices)]
    (when (and (~= d.port nil)
               (= d.name "16n"))
      (let [device {:name d.name
                    :port d.port}
            m (midi.connect d.port)
            cb (make-cb-16n)]
        (_16n.init cb)))))

(fn cb-kbd [keycode v]
  "Callback to bind to `keyboard.code'."
  (let [pgn (kbd->pgn keycode v)]
    (process-pgn pgn)))

(fn init-keyboard []
  (set keyboard.code cb-kbd))



;; FNS

;; (macro matches? [map search]
;;   `(let [_# nil]                        ; FIXME: `do' / `progn'?
;;      (var match?-res# false)
;;      (pprint ,search)
;;      (match ,map ,search                ; doesn't work as `match' is a macro as well
;;             ;; (set match?-res# true)
;;             (print "WTF???")
;;             )
;;      match?-res#))

(fn init []
  (init-grid)
  (init-midi-all)
  (init-keyboard)

  (set rules
       {
        {:chan :midi
         :dev {:name "bleached"}
         :msg {:type "cc"}}
        #(do (m/- $1 {:msg {:cc 100}})
             (pprint $1))
        ;;
        })

  )

(fn redraw []
  (screen.clear)

  (screen.move (/ screen-w 2)
               (/ screen-h 2))
  (screen.text "super")

  (screen.update))



;; MAIN

{:init init
 :key cb-btn
 :enc cb-enc
 :redraw redraw
 :pprint pprint}
