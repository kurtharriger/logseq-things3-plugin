(ns cljs-playground.core
  (:require
   [applied-science.js-interop :as j]
   [cljs-http.client :as http]
   [cljs.reader :refer [read-string]]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   ;[clojure.string]
   [clojure.walk :as walk]
   [com.rpl.specter :as s :refer-macros [select transform]]
   [datascript.core :as d]
   [com.wsscode.async.async-cljs :as wa :refer [go-promise  <? <?maybe]]
   [cljs-time.core :as t]
   [cljs-time.coerce :as tc]
   [cljs-time.format :as tf]
   ; multi-method needs import even if not used
   ^:ingore-unused [cljs-time.instant] 
   ))


(def debug (partial println "debug:"))
(def warn (partial println "warn:"))
;(def trace (partial println "trace:"))
(def trace (constantly nil))
; go and print 
(def last-gp (volatile! nil))
(defn gp [value & args]
  (go-promise (let [r (<?maybe
                       (if (ifn? value) (apply value args) value))]
                (vreset! last-gp r)
                (pprint r)
                r)))

; --- 

(defn logseq* [obj method & args]
  (let [jsargs (mapv clj->js args)
        v (j/get obj method)
        ;_ (trace (str (cons method jsargs)))
        _ (trace (pr-str (cons method args)))
        result (if-not (ifn? v) v
                       (apply (partial j/call obj method) jsargs))]

    (if (instance? js/Promise result)
      (go-promise
       (let [result (wa/<?maybe result)
             ;_ (trace method ">>" result) 
             result (js->clj result :keywordize-keys true)]
         (trace method ">" result)
         result))
      (let [;_ (trace method ">>" result) 
            result (js->clj result :keywordize-keys true)]
        (trace method ">" result)
        result))))

;; https://logseq.github.io/plugins/modules.html

; * methods have additional wrappers for additional 
; argument coersion 
(def ready* (partial logseq* js/logseq :ready))
(def settings (partial logseq* js/logseq :settings))

(def show-msg! (partial logseq* js/logseq.App :showMsg))
(def register-command*! (partial logseq* js/logseq.App :registerCommand))
(def register-command-palette*! (partial logseq* js/logseq.App :registerCommandPalette))
(def get-current-block (partial logseq* js/logseq.Editor :getCurrentBlock))
(def get-current-page* (partial logseq* js/logseq.Editor :getCurrentPage))
(def get-block (partial logseq* js/logseq.Editor :getBlock))
(def get-page (partial logseq* js/logseq.Editor :getPage))
(def get-page-blocks-tree (partial logseq* js/logseq.Editor :getPageBlocksTree))
(def register-slash-command* (partial logseq* js/logseq.Editor :registerSlashCommand))
(def create-page! (partial logseq* js/logseq.Editor :createPage))
(def insert-editing-at-cursor!  (partial logseq* js/logseq.Editor :insertAtEditingCursor))
(def insert-block! (partial logseq* js/logseq.Editor :insertBlock))
(def insert-batch-block! (partial logseq* js/logseq.Editor :insertBatchBlock))
; warn: query needs to be passed as string but cljs->js will convert it to a js-object 
; use pr-str on query when calling this method. Use datascript-query instead
(def datascript-query* (partial logseq* js/logseq.DB :datascriptQuery))

(defn get-current-page []
  ; for some reason get-current-page is sometimes returnning nil even when I'm on a journal page
  ; I thought maybe it was due to being journal page rather then regular page but it "should" work on 
  ; journal pages as well but I can't seem to rely on it so added this hack as a workaround 
  (go-promise (or (<?maybe (get-current-page*))
                  (do (warn "get-current-page returned nil") nil)
                  (<?maybe (get-page (get-in (<?maybe (get-current-block)) [:page :id]))))))


(def last-error (volatile! nil))
(defn displaying-errors [f]
  (fn [& args]
    (go-promise
     (<?maybe (try (<?maybe (apply f args))
                   (catch :default e
                     (debug "Caught Error: " e)
                     (j/call js/console :error e)
                     (vreset! last-error e)
                     (show-msg! (str "Error:\n"
                                     (pr-str e)))))))))

(defn with-promise-result [f]
  (fn [& args]
    (new js/Promise
         (fn [resolve reject]
           (go-promise (try (resolve (<?maybe (apply f args)))
                            (catch :default e (reject e))))))))


(defn ready [callback] (ready* (with-promise-result (displaying-errors callback))))
(defn register-command-palette! [opts callback]
  (register-command-palette*! opts (with-promise-result (displaying-errors callback))))

(defn register-slash-command! [text callback]
  (logseq* js/logseq.Editor :registerSlashCommand text (with-promise-result (displaying-errors callback))))



; datascript-query requires entire query to be sent as string and does not
; yet support arguments (and thus probably not rules) as workaround 
; use ::token-name in query and then pass in replacements to update the 
; query 
; see also https://github.com/tonsky/datascript/blob/1.3.5/src/datascript/built_ins.cljc#L40
; https://docs.xtdb.com/language-reference/1.20.0/datalog-queries/
(defn datascript-query [query & [replacements]]
  (debug [query replacements])
  (let [query-string (pr-str (clojure.walk/postwalk-replace replacements query))]
    (debug query-string)
    (datascript-query* query-string)))

;; ===============

(defn get-insert-opts [parent-block]
  (if (empty? (:content parent-block))
    {:sibling true :before true}
    {:sibling false}))

(defn g-insert-block!
  "Allows specifying a function for parent block
   if block is empty then content is inserted before it (as if inserted at point)
   if block is not empty then content is inserted as a child
   
   (g-insert-block! get-current-block \"testing\")
   "
  [parent-block content]
  (go-promise
   (<?maybe
    (let [parent-block (if (fn? parent-block) (parent-block) parent-block)
          parent-block (<?maybe parent-block)
          _ (debug :parent-block parent-block :uuid (:uuid parent-block))]
      (if parent-block
        (let [src-block (:uuid parent-block)
              opts (get-insert-opts parent-block)]
          (debug (pr-str "g-insert-block!:" [src-block content opts]))
          (insert-block! src-block content opts))
        (js/Error "parent block not found"))))))

;(set! g-insert-block! (displaying-errors g-insert-block!))

(defn get-current-journal-page-date
  "Returns date of currently active journal page or nil if not on a journal page"
  []
  ; get-current-page returns nil on journal page instead 
  ; calls get-page with page id of the current block to locate 
  ; journal page

  (go-promise
   (let [{page-property :page} (<?maybe (get-current-block))
         {page-id :id} page-property
         page (<?maybe (get-page page-id))
         ;journal day is yyyyMMdd stored as NUMBER
         {journalDay :journalDay} page]
     (when journalDay
       (tf/parse (tf/formatters :basic-date) (str journalDay))))))


(comment
  (gp (datascript-query '[:find (pull ?p [*]) :where [?p :block/journal-day 20220117]]))

  (go-promise (-> (<?maybe
                   (datascript-query '[:find (pull ?p [*]) . :where [?p :block/journal-day 20220117]]))
                  pprint)))


; note: block property in get-page is journalDay
; but in datascript it is :block/journal-day
(defn get-journal-page-for-date* [date]
  (let [journal-day (int (tf/unparse (tf/formatters :basic-date) date))
        query '[:find (pull ?p [*]) .
                :where
                [?p :block/journal-day ::journal-day]
                [(missing? $ ?p :block/parent)]
                ;the missing parent should be enough, but during development I've managed to 
                ; occassionally create orphan blocks that are assigned to page but not visible
                ; and these are being picked up instead and then get-page returns nil because
                ; block returned here was not a page
                ;[?p :block/file ?f]
                ]]
    (datascript-query query {::journal-day journal-day})))

; there seems to be a bug in logseq where uuid does not 
; get serialized correctly.  May not be an issue but
; as workaround just get page id and then query the page

(defn get-journal-page-for-date [date]
  (go-promise (let [response (<?maybe (get-journal-page-for-date* date))
                    {page-id :id} response]
                (<?maybe (get-page page-id)))))


(comment
  (set! cljs-playground.core/get-journal-page-for-date (displaying-errors get-journal-page-for-date))
  (go-promise
   (pprint (<?maybe (get-journal-page-for-date (t/today))))))

; ---

(defonce db
  (d/create-conn
   {:things.area/uuid {:db/unique :db.unique/identity}
    :things.task/uuid {:db/unique :db.unique/identity}
    :things.checklist/uuid  {:db/unique :db.unique/identity}
    :things.task/area {:db/valueType :db.type/ref}
    :things.task/project {:db/valueType :db.type/ref}
    :things.checklist/task {:db/valueType :db.type/ref}}))


(defn go-get-data-from-proxy []
  (go-promise (:body (<? (http/get "http://localhost:7980")))))


(defn go-load-db! []
  (go-promise (d/transact! db (<? (go-get-data-from-proxy)))
              (debug (str "Datum count " (count (:eavt @db))))))


(defn get-areas []
  (d/q '[:find [(pull ?a [*]) ...]
         :where [?a :things.area/uuid]] @db))

(defn get-projects []
  (d/q '[:find [(pull ?p [*]) ...]
         :where [?t :things.task/project ?p]] @db))


(defn create-block-for-area [area]
  (let [{:things.area/keys [title uuid]} area]
    (str title "\nthings.area/uuid:: " uuid)))

(defn with-status-marker [status title]
  (str (get {0 "TODO "
             1 "LATER " ; Scheduled? 
             2 "CANCELLED "
             3 "DONE "} status "") title))

(defn create-block-for-checklist [checklist]
  (let [{:things.checklist/keys [title uuid status]} checklist]
    (str (with-status-marker status title) "\nthings.checklist/uuid:: " uuid)))

; to match scheduled date string format
; from-date seems like it shoudl be unnecesary but getting goog.date objects back from datascript and/or parsing edn from proxy
; todo; determine if has time and drop hours in some cases getting .getHOurs undefined but not always as seeing times 00:00

(defn to-date-string [instant]
  (try 
    (tf/unparse (tf/formatter "yyyy-MM-dd E HH:mm") (tc/from-date instant))
    ; d.getHours is undefined
    (catch js/Error e 
      (tf/unparse (tf/formatter "yyyy-MM-dd") (tc/from-date instant)))))

(defn create-block-for-task [task]
  (let [{:things.task/keys [title uuid status startDate stopDate project]} task
        project-title (:things.task/title project)
        area-title (get-in project [:things.task/area :things.area/title])]
    (str (with-status-marker status title) "\nthings.task/uuid:: " uuid
         (and startDate (str "\nthins.task/startDate:: " (to-date-string startDate)))
         (and stopDate (str "\nthings.task/stopDate:: " (to-date-string stopDate)))
         ; projects are also tasks so some ambiguity in things.task/title 
         ; changing to things.project/title here... 
         ; todo consider creating project as item in db
         (and project-title (str "\nthings.project/title:: [[" project-title "]]"))
         (and project-title (str "\nthings.area/title:: [[" area-title "]]")))))

(defn get-completed-tasks [date]
  (->>
   (d/q '[:find
          [(pull ?t
                 [* {:things.checklist/_task [*]

                     :things.task/project [:things.task/title
                                           {:things.task/area [:things.area/title]}]}]) ...]
          :in $ ?from ?to
          :where
          [?t :things.task/uuid]
          [?t :things.task/stopDate ?sd]
          [(<= ?from ?sd)]
          [(< ?sd ?to)]]
        @db
        (tc/to-date date)
        (t/plus date (t/days 1)))
   (s/setval [s/ALL (s/map-key :things.checklist/_task)] :things.task/checklist)))

(comment

  (->> (get-completed-tasks (t/today))
       (mapv create-block-for-task)
       (pprint)))

(defn find-existing-tasks-on-page [page]
  (go-promise
   (let [page (<?maybe (if (fn? page) (page) page))
         matching-block-properties
         (<?maybe (datascript-query
                   '[:find [?u ...]
                     :where
                     [?b :block/properties ?p]
                     [(get ?p :things.task/uuid) ?u]
                     [(contains? ?p :things.task/uuid)]
                     [?b :block/page ::page]]
                   {::page (:id page)}))]
     matching-block-properties)))

;(set! find-existing-tasks-on-page (displaying-errors find-existing-tasks-on-page))
;(gp find-existing-tasks-on-page get-current-page)

(defn get-or-create-logbook-heading-block! [journal-page]
  (go-promise
   (<?maybe
    (let [journal-page (<?maybe (if (fn? journal-page) (journal-page) journal-page))
          {journal-page-id :id} journal-page]
      (if-let [block-id (<?maybe
                         (datascript-query
                          '[:find  ?b .
                            :where
                            [?b :block/page ::journal-page-id]
                            [?b :block/content ?c]
                            [(clojure.string/includes? ?c "Things Logbook")]]
                          {::journal-page-id journal-page-id}))]
        (get-block block-id)
        (insert-block! (:name journal-page) "[[Things Logbook]]" {:isPageBlock true}))))))

(defn insert-task-children! [task-block task]
  (go-promise
   (<?maybe
    (let [notes  (:things.task/notes task)
          checklists (:things.task/checklist task)]
      ;(debug (pr-str [::notes notes ::task task ::task-block]))
      (when-not (empty? notes) (<?maybe (g-insert-block! task-block notes)))
      (doseq [checklist checklists
              :let [block (create-block-for-checklist checklist)]]
        (<?maybe (g-insert-block! task-block block)))))))

; (set! get-or-create-logbook-heading-block! (displaying-errors get-or-create-logbook-heading-block!))
(defn insert-completed-tasks-for-current-journal-page!
  []
  (go-promise
   (if-let [date (<?maybe (get-current-journal-page-date))]
     (let [journal-page (<?maybe (get-journal-page-for-date date))
           _ (assert journal-page "Journal page is nil, try reindex")
           _ (debug :journal-page journal-page)
           tasks (seq (<?maybe (get-completed-tasks date)))
           tasks-on-page (<?maybe (find-existing-tasks-on-page journal-page))
           {tasks-to-insert false tasks-to-update true}
           (group-by  #(contains? tasks-on-page (:things.task/uuid %)) tasks)]
       (let [heading-block (<?maybe (get-or-create-logbook-heading-block! journal-page))]
         (doseq [task tasks-to-insert]
           (let [block (create-block-for-task task)
                 task-block (<?maybe (g-insert-block! heading-block block))]
             (debug :task task :task-block)
             (insert-task-children! task-block task)
             task-block)
           ;todo: maybe update existing tasks? in theory if they are done they shouldn't change
           ;todo: should I check to see if user uncompleted task and delete it
           )
         
         (show-msg! (str "Logbook updated"))))
     (show-msg! "This command must be run from a journal page"))))


(comment
  (set! insert-completed-tasks-for-current-journal-page! (displaying-errors insert-completed-tasks-for-current-journal-page!))
  (gp insert-completed-tasks-for-current-journal-page!))


(defn find-logseq-things-area-blocks []
  (datascript-query '[:find (pull ?b [*])
                      :where [?b :block/properties ?p]
                      [(contains? ?p :things.area/uuid)]]))

(defn find-logseq-things-task-blocks []
  (datascript-query '[:find (pull ?b [*])
                      :where [?b :block/properties ?p]
                      [(contains? ?p :things.task/uuid)]]))

;; =============


; for now reloads data from things db proxy
; before running the command. Intended to wrap top
; level commands to ensure data is current
; could potentially skip reload if data was recently loaded
; or ideally implement subscription for real time updates
(defn with-things-db [f]
  (fn [& args]
    (go-promise (<?maybe (go-load-db!))
                (<?maybe (apply f args)))))

(defn main []
  (register-slash-command! "Insert Things Logbook"
                           (with-things-db insert-completed-tasks-for-current-journal-page!))
  (register-command-palette! {:key "insert-things-logbook" :label "Insert Things Logbook" :keybinding "Shift+Cmd+L"}
                             (with-things-db insert-completed-tasks-for-current-journal-page!)))

(defn init []
  (ready  main))


(comment
  (count (:eavt @db))
  (d/q '[:find (count ?e) .  :where [?e :things.area/uuid]] @db)
  (d/q '[:find (count ?e) .  :where [?e :things.task/uuid]] @db)
  (d/q '[:find (count ?e) .  :where [?e :things.checklist/uuid]] @db)


  ; structured pull but no filters on pulll
  (-> (d/q  '[:find
              [(pull ?a [:things.area/title
                         {:things.task/_area
                          [:things.task/title
                           {:things.task/_project [:things.task/title]}]}]) ...]
              :where
              [?a :things.area/title "Family"]
              ;[?t :things.task/area ?a]
              ;[?c :things.task/project ?t]
              ]
            @db)
      pprint)

  (-> (d/q  '[:find
              (pull ?a [:things.area/title {:things.task/_area (?t [:things.task/title])}])
              :where
              [?a :things.area/title "Family"]
              [?t :things.task/area ?a]]
            @db)
      pprint)



  (->
   (q '[:find (pull ?b [*])
        :where [?b :block/properties ?p]
        [(contains? ?p :things.task/uuid)]]
      pprint))



  (gp datascript-query '[:find [(pull ?b [*]) ...] :where
                         (or-join [?b ?m]
                                  (and [?b :block/marker ?m]
                                       [(contains? #{"NOW" "LATER"} ?m)]
                                       [?b :block/scheduled ?d])
                                  (and [?b :block/marker ?m]
                                       [(contains? #{"NOW"} ?m)]))])

  ;
  ;
  )
