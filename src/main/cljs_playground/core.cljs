(ns cljs-playground.core
  (:require
   [applied-science.js-interop :as j]
   [cljs.core :refer [js->clj]]
   [cljs-http.client :as http]
   [cljs.reader :refer [read-string]]
   [clojure.pprint :refer [pprint]]
   [clojure.string]
   [com.rpl.specter :as s :refer-macros [select transform]]
   [datascript.core :as d]
   [com.wsscode.async.async-cljs :as wa :refer [go <?]]))


(defn gprint [value] (go (pprint (wa/<?maybe value))))
(defn logseq* [obj method & args]
  (let [jsargs (mapv clj->js args)
        v (j/get obj method)
        r (if-not (ifn? v) v
                  (apply (partial j/call obj method) jsargs))]
  
    (if (instance? js/Promise r)
      (go (js->clj (wa/<?maybe r) :keywordize-keys true))
      (js->clj v :keywordize-keys true))))

(def ready (partial logseq* js/logseq :ready))
(def settings (partial logseq* js/logseq :settings))
(def get-current-page (partial logseq* js/logseq.Editor :getCurrentPage))
(def get-page (partial logseq* js/logseq.Editor :getPage))
(def get-page-blocks-tree (partial logseq* js/logseq.Editor :getPageBlocksTree))

(def show-msg! (partial logseq* js/logseq.App :showMsg))
(def create-page! (partial logseq* js/logseq.Editor :createPage))
(def insert-editing-at-cursor!  (partial logseq* js/logseq.Editor :insertAtEditingCursor))
(def insert-block! (partial logseq* js/logseq.Editor :insertBlock))
(def insert-batch-block! (partial logseq* js/logseq.Editor :insertBatchBlock))
(def datascript-query* (partial logseq* js/logseq.DB :datascriptQuery))
(defn q [query] (datascript-query* (pr-str query)))

(defn with-promise-result [f]
  (fn [& args]
    (new js/Promise
         (fn [resolve reject]
           (go (try (resolve (wa/<?maybe (apply f args)))
                    (catch :default e (reject e))))))))


(defn register-slash-command! [text callback]
  (logseq* js/logseq.Editor :registerSlashCommand text (with-promise-result callback)))

(defn ensure-page! [page-name]
  (go
    (or (<? (get-page page-name)) (<? (create-page! page-name)))))

(defn create-sample-page! []
  (go (let [page (<? (ensure-page! "Clojure Plugin"))
            {:keys [uuid]} page]
        (<? (insert-batch-block! uuid [{:content "testing" :sibling false}]))
        (<? (show-msg! "Updated Clojure Plugin page")))))

(defn main []
  (go
    (<? (register-slash-command! "Clojure Slash" create-sample-page!))
    (<? (show-msg! "Hello from Clojure!"))))


(defn init []
  (go (try (ready main)
           (catch :default e (j/call js/console :error e)))))


(defn find-logseq-things-area-blocks []
  (q '[:find (pull ?b [*])
       :where [?b :block/properties ?p]
       [(contains? ?p :things.area/uuid)]]))

(defn find-logseq-things-task-blocks []
      (q '[:find (pull ?b [*])
           :where [?b :block/properties ?p]
           [(contains? ?p :things.task/uuid)]]))


(defonce db
  (d/create-conn
   {:things.area/uuid {:db/unique :db.unique/identity}
    :things.task/uuid {:db/unique :db.unique/identity}
    :things.checklist/uuid  {:db/unique :db.unique/identity}
    :things.task/area {:db/valueType :db.type/ref}
    :things.task/project {:db/valueType :db.type/ref}
    :things.checklist/task {:db/valueType :db.type/ref}}))


(defn get-areas []
  (d/q '[:find (pull ?a [*])
         :where [?a :things.area/uuid]] @db))

(defn get-projects []
  (d/q '[:find (pull ?p [*])
         :where [?t :things.task/project ?p]] @db))


(defn go-get-data-from-proxy []
  (go (:body (<? (http/get "http://localhost:7980")))))


(defn go-load-db! []
  (go (d/transact! db (<? (go-get-data-from-proxy)))))


(def page-name "Things")


(comment

  (-> (d/q '[:find ?e  :where [?e :things.area/uuid]] @db)
      count)
  (-> (d/q '[:find ?e  :where [?e :things.task/uuid]] @db)
      count)
  (-> (d/q '[:find ?e  :where [?e :things.checklist/uuid]] @db)
      count)

  (-> (d/q  '[:find (pull ?a [*]) .
              :where [?a :things.area/title "Family"]]
            @db)
      pprint)


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


