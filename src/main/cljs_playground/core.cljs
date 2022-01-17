(ns cljs-playground.core
  (:require
   [cljs.core.async :as async]
   [clojure.walk :as walk]
   [applied-science.js-interop :as j]
   [cljs.core :refer [js->clj]]
   [cljs-http.client :as http]
   [cljs.reader :refer [read-string]]
   [clojure.pprint :refer [pprint]]
   [clojure.string]
   [com.rpl.specter :as s :refer-macros [select transform]]
   [datascript.core :as d]
   [com.wsscode.async.async-cljs :as wa :refer [go-promise  <? <?maybe]]
   [tick.core :as tick]
   ["moment" :as moment]
   [cljs-time.core :as t]
   [cljs-time.coerce :as tc]
   [cljs-time.format :as tf]
   ))

(def debug (partial println "debug:"))
;(def trace (partial println "trace:"))
(def trace (constantly nil))


(defn gp [value & args]
  (go-promise (pprint (wa/<?maybe
               (if (ifn? value) (apply value args) value)))))

(defn logseq* [obj method & args]
  (let [jsargs (mapv clj->js args)
        v (j/get obj method)
        _ (trace "(" method (mapv pr-str args) ")")
        _ (trace "(" method (mapv pr-str jsargs) ")")
        result (if-not (ifn? v) v
                  (apply (partial j/call obj method) jsargs))
         ]
    (trace method ">>" result)
    (if (instance? js/Promise result)
      (go-promise
       (let [result (js->clj (wa/<?maybe result) :keywordize-keys true)]
         (trace method ">" result)
         result))
      (let [result (js->clj result :keywordize-keys true)]
        (trace method ">" result)
        result))))

(defn with-promise-result [f]
  (fn [& args]
    (new js/Promise
         (fn [resolve reject]
           (go-promise (try (resolve (wa/<?maybe (apply f args)))
                    (catch :default e (reject e))))))))

;; https://logseq.github.io/plugins/modules.html

(def ready (partial logseq* js/logseq :ready))
(def settings (partial logseq* js/logseq :settings))
(def get-current-block (partial logseq* js/logseq.Editor :getCurrentBlock))
(def get-current-page (partial logseq* js/logseq.Editor :getCurrentPage))
(def get-page (partial logseq* js/logseq.Editor :getPage))
(def get-page-blocks-tree (partial logseq* js/logseq.Editor :getPageBlocksTree))

(def show-msg! (partial logseq* js/logseq.App :showMsg))
(def create-page! (partial logseq* js/logseq.Editor :createPage))
(def insert-editing-at-cursor!  (partial logseq* js/logseq.Editor :insertAtEditingCursor))
(def insert-block! (partial logseq* js/logseq.Editor :insertBlock))
(def insert-batch-block! (partial logseq* js/logseq.Editor :insertBatchBlock))

; note: query needs to be passed as string but cljs->js will convert it to a map
(def datascript-query* (partial logseq* js/logseq.DB :datascriptQuery))

; datascript-query requires entire query to be sent as string and does not
; yet support arguments (and thus probably not rules) as workaround 
; use ::token-name in query and then pass in replacements to update the 
; query eg
(defn datascript-query [query & [replacements]] 
  (debug [query replacements])
  (let [query-string (pr-str (clojure.walk/postwalk-replace replacements query))]
    (debug query-string)
    (datascript-query* query-string)))

(defn register-slash-command! [text callback]
  (logseq* js/logseq.Editor :registerSlashCommand text (with-promise-result callback)))

(def last-error (volatile! nil))
(defn displaying-errors [f]
  (fn [& args]
    (go-promise (try (<?maybe (apply f args))
                     (catch :default e
                       (debug "Caught Error: " e)
                       (j/call js/console :error e)
                       (vreset! last-error e)
                       (show-msg! (str "Error:\n" 
                                       (pr-str e))))))))


(defn ensure-page! [page-name]
  (go-promise
    (or (<? (get-page page-name)) (<? (create-page! page-name)))))

(defn create-sample-page! []
  (go-promise (let [page (<? (ensure-page! "Clojure Plugin"))
            block (<? (insert-block! "Clojure Plugin" "Content" {:isPageBlock true}))
            {:keys [uuid]} block]

        (<? (insert-batch-block!
             uuid
             [{:content "Child 1"}
              {:content "Child 2"
               :children [{:content "Grandchild"}]}]))

        (<? (show-msg! "Updated Clojure Plugin page")))))

(defn main []
  (go-promise
    (<? (register-slash-command! "Clojure Slash" create-sample-page!))
    (<? (show-msg! "Hello from Clojure!"))))

(defn init []
  (go-promise (try (ready main)
           (catch :default e (j/call js/console :error e)))))



(defn get-insert-opts [parent-block]
  (if (empty? (:content parent-block))
    {:sibling true :before true}
    {:sibling false}))

(defn g-insert-block! [parent-block content]
  (go-promise
   (<?maybe
    (let [parent-block (if (ifn? parent-block) (parent-block) parent-block)
          parent-block (<?maybe parent-block)]
      (if parent-block
        (let [src-block (:uuid parent-block)
              opts (get-insert-opts parent-block)]
          (debug (pr-str "g-insert-block!:" [src-block content opts]))
          (insert-block! src-block content opts))
        (js/Error "parent block not found"))))))

(defn get-current-journal-page-date
  "Returns date of currently active journal page or nil if not on a journal page"
  []
  ; get-current-page returns nil on journal instead use
  ; get page with page id in current  block

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
                  pprint))

  )


; note: block property in get-page is journalDay
; but in datascript it is :block/journal-day
(defn get-journal-page-for-date* [date]
  (let [journal-day (int (tf/unparse (tf/formatters :basic-date) date))
        query '[:find (pull ?p [*]) .
                :where [?p :block/journal-day ::journal-day]]]
    (datascript-query query {::journal-day journal-day})))

; there seems to be a bug in logseq where uuid does not 
; get serialized correctly.  May not be an issue but
; as workaround just get page id and then query the page

(defn get-journal-page-for-date [date]
  (go-promise (let [response (<?maybe (get-journal-page-for-date* date))
                    {page-id :id} response]
                (<?maybe (get-page page-id)))))

(set! cljs-playground.core/get-journal-page-for-date (displaying-errors get-journal-page-for-date))
(comment 
  (go-promise 
   (pprint (<?maybe (get-journal-page-for-date (t/today))))
       )
  )
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
  (str (get {2 "CANCELLED "
             3 "DONE "} status "") title))

(defn create-block-for-checklist [checklist]
  (let [{:things.area/keys [title uuid status]} checklist]
    (str title "\nthings.checklist/uuid:: " uuid)))

(defn create-block-for-task [task]
  (let [{:things.task/keys [title uuid status]} task]
    (str (with-status-marker status title) "\nthings.task/uuid:: " uuid)))

(defn get-completed-tasks [date]
  (d/q '[:find
         [(pull ?t
                [*]) ...]
         :in $ ?from ?to
         :where
         [?t :things.task/uuid]
         [?t :things.task/stopDate ?sd]
         [(<= ?from ?sd)]
         [(< ?sd ?to)]] 
       @db 
       (tc/to-date date) 
       (t/plus date (t/days 1))))

(comment

  (->> (get-completed-tasks (t/today))
       (mapv create-block-for-task)
       (pprint))
)
  

(defn insert-completed-tasks-for-current-journal-page!
  []
  (go-promise
   (if-let [date (<?maybe (get-current-journal-page-date))]
     (if-let [tasks (<?maybe (get-completed-tasks date))]
       (doseq [block (mapv create-block-for-task tasks)]
         (debug block)
         (<?maybe (g-insert-block! get-current-block block)))
            ; todo format date
       (show-msg! (str "No tasks marked complete for " date)))
     (show-msg! "This command must be run from a journal page at the desired insertion point"))))

(set! insert-completed-tasks-for-current-journal-page! 
      (displaying-errors insert-completed-tasks-for-current-journal-page!))

(comment
  (insert-completed-tasks-for-current-journal-page!)
  )


(def page-name "Things")

;; ----

(defn find-logseq-things-area-blocks []
  (datascript-query '[:find (pull ?b [*])
       :where [?b :block/properties ?p]
       [(contains? ?p :things.area/uuid)]]))

(defn find-logseq-things-task-blocks []
  (datascript-query '[:find (pull ?b [*])
       :where [?b :block/properties ?p]
       [(contains? ?p :things.task/uuid)]]))


(comment
  (count (:eavt @db))
  (d/q '[:find (count ?e) .  :where [?e :things.area/uuid]] @db)
  (d/q '[:find (count ?e) .  :where [?e :things.task/uuid]] @db)
  (d/q '[:find (count ?e) .  :where [?e :things.checklist/uuid]] @db)
  

  ; structured pull but no filters on pulll
  (-> (d/q  '[:find
              (pull ?a [:things.area/title
                        {:things.task/_area
                         [:things.task/title
                          {:things.task/_project [:things.task/title]}]}])
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




  ;
  )
