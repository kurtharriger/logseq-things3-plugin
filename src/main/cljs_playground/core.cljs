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
(def datascriptQuery (partial logseq* js/logseq.DB :datascriptQuery))

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
    (<? (show-msg! "Hello from Clojure!"))
    (<? (create-sample-page!))))


(defn init []
  (go (try (ready main)
           (catch :default e (j/call js/console :error e)))))
