(require '[clojure.string :as str])

(defn read-digit [c] (Character/digit c 10))
(defn parse-line [line] (map read-digit line))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        rows (map parse-line lines)
        height (count rows)
        width (count (first rows))
        indices (map (fn [i] [(rem i width) (quot i width)]) (range (* width height)))]
    (zipmap indices (flatten rows))))

(defn apply-offset [pos offset] (mapv + pos offset))

(def all-neighbours [[-1 0] [1 0] [0 -1] [0 1]])

(defn neighbours [pos data]
  (map (fn [offset] (get data (apply-offset pos offset) 9)) all-neighbours))

(defn risk-level [height-map]
  (let [low-points (filter (fn [[pos value]] (every? (partial < value) (neighbours pos height-map))) height-map)
        low-points-risk (map (comp inc second) low-points)]
    (reduce + low-points-risk)))

(if (not= (count *command-line-args*) 1) 
  ((binding [*out* *err*] (println "Usage: clojure -M one.clj <input>"))
   (System/exit 1)) ())

(def file-name (first *command-line-args*))
(def height-map (parse-input (slurp file-name)))
(println (risk-level height-map))