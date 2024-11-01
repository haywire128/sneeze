(ns haywire128.sneeze
  (:require
    [clojure.string :as str]))

(defn parse-keyword [kw]
  (let [tag-str (name kw)
        parts (re-seq #"[^#.]+|[#.][^#.]*" tag-str)
        tag (first parts)
        id (some #(when (re-matches #"#([a-zA-Z0-9-]+)" %) (subs % 1)) parts)
        classes (map #(when (re-matches #"\.([a-zA-Z0-9-]+)" %) (subs % 1)) parts)]
    (if (> (count (filter #(re-matches #"#([a-zA-Z0-9-]+)" %) parts)) 1)
      (throw (Exception. "Invalid input: more than one ID specified"))
      {:tag tag
       :id id
       :class (vec (filter some? classes))})))

(defn tag-name->map [tag-name] (merge {:extra-attrs {} :content nil} (#'parse-keyword tag-name)))

(defn form->map [form] (if (vector? form) 
                         (if (= clojure.lang.PersistentArrayMap (type (second form))) 
                           (let [contents (subvec form 2)]
                             (if (not (= clojure.lang.LazySeq (type (first contents))))
                               (assoc (#'tag-name->map (first form)) :extra-attrs (second form) :content (mapv form->map contents))
                               (assoc (#'tag-name->map (first form)) :extra-attrs (second form) :content (apply mapv form->map contents)))) 
                           (let [contents (subvec form 1)]
                             (if (not (= clojure.lang.LazySeq (type (first contents))))
                               (assoc (#'tag-name->map (first form)) :content (mapv form->map contents))
                               (assoc (#'tag-name->map (first form)) :content (apply mapv form->map contents)))))
                         (str form)))

(defn stringify-extra-attrs [attrs]
  (->> attrs
       (map (fn [[k v]] (str (name k) " = \"" v "\"")))
       (clojure.string/join " ")))

(defn stringify-class [class] (if (empty? class) 
                                nil 
                                (clojure.string/join " " class)))

(defn map->html [{:keys [tag id class extra-attrs content]}]
  (str "<" tag
       (when id (str " id = \"" id "\""))
       (when (seq class) (str " class = \"" (#'stringify-class class) "\""))
       (when (seq extra-attrs) (str " " (#'stringify-extra-attrs extra-attrs)))
       ">"
       (apply str (map #(if (map? %) (map->html %) %) content))
       "</" tag ">"))


(defn sneeze [form]
  (let [form-map (form->map form)]
    (map->html form-map)))