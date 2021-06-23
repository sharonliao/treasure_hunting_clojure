(ns treasure)

(defn splitStr
  [line]
  (clojure.string/split line #"")
  )

(defn readFileToMatrix []
  (with-open [rdr (clojure.java.io/reader "map.txt")]
    (into [] (map splitStr (reduce conj [] (line-seq rdr))))))


(defn getChar [a b matrix]
  (get (get matrix a) b)
  )

(defn checkMatrix [matrix]
  (loop [cloSet #{}
         rowNum (count matrix)]
    (if (= rowNum 0) (= (count cloSet) 1)
                     (recur (conj cloSet (count (get matrix (- rowNum 1) ))) (dec rowNum))
                     )
    )
  )


(defn move? [i j matrix]
  (or (= (getChar i j matrix) "-") (= (getChar i j matrix) "@"))
  )

(defn goBack？ [i j matrix]
  (= (getChar i j matrix) "+")
  )

(defn printMap [matrix]
  (doseq [e matrix]
    (doseq [x e] (print x))
    (println "")
    ))


(defn updateMaze [row col flag maze]
  (assoc-in maze [row col] flag)
  )

(defn FindTreasure [matrix cnt]
  (loop [row 0
         col 0
         no 0
         maze matrix
         count cnt]
    (cond
      (zero? count) maze
      (= (getChar row col maze) "@") (do (println "Woo hoo, I found the treasure :-) \n ") maze)
      (= (getChar row (+ 1 col) maze) "@") (do (println "Woo hoo, I found the treasure :-) \n ") (updateMaze row col "+" maze))
      (= (getChar row (- col 1) maze) "@") (do (println "Woo hoo, I found the treasure :-) \n") (updateMaze row col "+" maze))
      (= (getChar (- row 1) col maze) "@") (do (println "Woo hoo, I found the treasure :-) \n") (updateMaze row col "+" maze))
      (= (getChar (+ row 1) col maze) "@") (do (println "Woo hoo, I found the treasure :-) \n") (updateMaze row col "+" maze))
      :else (cond
              (move? row (+ 1 col) maze) (recur row
                                                 (+ 1 col)
                                                 (inc no)
                                                 (updateMaze row col "+" maze)
                                                 (dec count)
                                                 )
              (move? row (- col 1) maze) (recur row
                                                 (- col 1)
                                                 (inc no)
                                                 (updateMaze row col "+" maze)
                                                 (dec count)
                                                 )
              (move? (- row 1) col maze) (recur (- row 1)
                                                 col
                                                 (inc no)
                                                 (updateMaze row col "+" maze)
                                                 (dec count)
                                                 )
              (move? (+ row 1) col maze) (recur (+ row 1)
                                                 col
                                                 (inc no)
                                                 (updateMaze row col "+" maze)
                                                 (dec count)
                                                        )
              (goBack？ row (+ 1 col) maze) (recur row
                                                  (+ 1 col)
                                                  (inc no)
                                                  (updateMaze row col "!" maze)
                                                  (dec count)
                                                  )
              (goBack？ row (- col 1) maze) (recur row
                                                  (- col 1)
                                                  (inc no)
                                                  (updateMaze row col "!" maze)
                                                  (dec count)
                                                  )
              (goBack？ (- row 1) col maze) (recur (- row 1)
                                                  col
                                                  (inc no)
                                                  (updateMaze row col "!" maze)
                                                  (dec count)
                                                  )
              (goBack？ (+ row 1) col maze) (recur (+ row 1)
                                                  col
                                                  (inc no)
                                                  (updateMaze row col "!" maze)
                                                  (dec count)
                                                  )

              :else ( do (println "Uh oh, I could not find the treasure :-(" "\n")
                      (updateMaze 0 0 "!" maze)
                      )
              )
      )
    ))


(defn checkAndFind [maze]
  (if (checkMatrix maze) (printMap (FindTreasure (readFileToMatrix) 2000))
                         (println "Map is not correct" )
                         ) )

(println "This is my challenge:")
(println )
(printMap (readFileToMatrix))
(println )
(checkAndFind (readFileToMatrix))