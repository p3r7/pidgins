

;; DEPS

(local char_modifier (require :core.keymap.char_modifier))
(local _16n (require :pidgins.lib._16n))
(local inspect (require :pidgins.lib.inspect))

(local s (require :sequins))


;; CONSTS

(local screen-w 128)
(local screen-h 64)

(local chan-btn :btn)
(local chan-enc :enc)
(local chan-hid :hid)
(local chan-hid-keyboard :hid/keyb)
(local chan-midi :midi)
(local chan-midi-16n :midi/16n)



;; CONF

(var rules {})


;; CORE

(fn pprint [v]
  (print (inspect v)))

;; (fn flatten [t]
;;   (let [out {}]
;;     (each [k v (pairs t)]
;;       (print "-------------")
;;       (print (.. k " => " (inspect v)))
;;       (if (= (type v) "table")
;;           (let [{[a b]} (flatten v)]
;;             (print (.. "TABLE " (inspect a)))
;;             ;; FIXME: there's no way this would work
;;             ;; (tset out [k (table.unpack k-path)] v))
;;             (tset out k v))
;;           (tset out [k] v)
;;           (print "NOT TABLE")))
;;     out))



;; ENRICHMENT / NORMALIZATION

(fn btn->pgn [id state]
  {:chan chan-btn
   :msg {:i id
         :state state}})

(fn enc->pgn [id delta]
  {:chan chan-enc
   :msg {:id id
         :delta delta}})

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


(fn process-pgn [pgn]
  ;; (match pgn rules)
  (match pgn

    {:chan :midi
     :dev {:name "bleached"}
     :msg {:type "cc"}} (pprint pgn)

     {:chan :hid/keyb} (pprint pgn))

  )

;; (fn pgn/+/2 [e1 e2]
;;   )

;; (fn pgn/+ [& es]
;;   (accumulate [out nil
;;                i n (ipairs es)]

;;     ))



;; LISTENER FNs

(fn cb-kbd [keycode v]
  "Callback to bind to `keyboard.code'."
  (let [pgn (kbd->pgn keycode v)]
    ;; TODO: register it
    (pprint pgn)))

(fn init-keyboard []
  (set keyboard.code cb-kbd))

(fn make-cb-midi [device]
  (fn [d]
    (let [pgn (midi->pgn device d)]
      ;; (pprint pgn)
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



;; FNS

(fn init []
  (init-midi-all)
  (init-keyboard)

  (set rules
       {
        {:chan :midi
         :dev {:name "bleached"}
         :msg {:type "cc"}} (print "YES")
        ;; #(pprint $1)
        ;; #(- $1 {:msg {:cc 100}})
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
 :redraw redraw}
