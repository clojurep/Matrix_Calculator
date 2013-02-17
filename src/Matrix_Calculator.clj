(ns Matrix-Calculator)



(defn matrix? [mat]

  (and 
      (not (nil? mat))
      (or (vector? mat) (list? mat) (seq? mat))
      (or (apply = true (map vector? mat)) (apply = true (map list? mat)))
      (or (empty? mat) (apply = (map #(= (count (first mat)) (count %)) mat)))
    )
  )
 

(defn sameSize? [mat1 mat2]
  {:pre [(matrix? mat1) (matrix? mat2)] }
   
   (and
     (= (count mat1) (count mat2))
     (= (count (first mat1)) (count (first mat2)))
    )
  ) 

(defn sameColsRows? [mat1 mat2]
  {:pre [(matrix? mat1) (matrix? mat2)] }
    
    (or (= (count (first mat1)) (count mat2)) (empty? mat1) (empty? mat2))
  )


(defn matTranspose [mat1]
  {:pre [(matrix? mat1)] }
   
   (vec (apply map vector mat1))
 )


(defn isSquare? [mat1]
  {:pre (matrix? mat1) }
  (sameColsRows? mat1 mat1)
  )


(defn sumVector [vec1]
  {:pre [(vector? vec1)] }
  
   (apply + vec1)
 )

(defn sumMatrix [mat1]
  {:pre [(matrix? mat1)] }
  
   (apply + (apply concat mat1))
  )

(defn createRandVec [n range]
  
  (loop [v (vector) i 0]
    (if (= i n)
      v
      
     (recur (conj v (rand-int range)) (inc i))
    )
   )
  )


(defn createRandMat [n m range]
  
  (loop [mat (vector) i 0]
    (if (= i n)
      mat
      
     (recur (conj mat (createRandVec m range)) (inc i))
    )
   )
  ) 
  

(defn addVectors [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ] }
   
    (vec 
      (lazy-seq
        (when-let [v1 (seq vec1)]
          (when-let [v2 (seq vec2)]
            (map + v1 v2)
      )
     )
    )
   )
  )

(defn addMatrixes [mat1 mat2]
  {:pre [(sameSize? mat1 mat2)] }
   
   (vec 
     (lazy-seq
       (when-let [m1 (seq mat1)]
         (when-let [m2 (seq mat2)]
           (pmap addVectors m1 m2)
       )
      )
     )
    )
   )
 

(defn multipicationVec [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ]  }
     
     (vec (map * vec1 vec2))
 )

(defn dotProduct [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ]  }
    
     (apply + (map * vec1 vec2))
)


(defn mulBy [s]
  #(* % s)
 )


(defn vecScalarMul [vec1 s]
  {:pre [(vector? vec1)] }
   
   (lazy-seq
     (when-let [v1 (seq vec1)]
       (map (mulBy s) v1)
    )
   )
 )

(defn matScalarMul [mat1 s]
  {:pre [(matrix? mat1)] }
   
   (lazy-seq
      (when-let [m1 (seq mat1)]
          (pmap #(vecScalarMul % s) m1)
     )
   )
 )


(defn subMatrixes [mat1 mat2]
  {:pre [(sameSize? mat1 mat2)] }
  
   (addMatrixes mat1 (matScalarMul mat2 -1)) 
 )


(defn matMultipication [mat1 mat2]
  {:pre [(sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
      nil
    
     (vec
       (loop [rslt (vector) m1 mat1]
         (if (= (count m1) 0)
            rslt
      
            (recur
              (conj rslt
                 (vec (pmap #(dotProduct (first m1) %)
                        (matTranspose mat2)))) (rest m1))
            )      
          )
        )
      )
    )


(defn matMultipication_lazy [mat1 mat2]
  {:pre [(sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
     nil    
    
     (lazy-seq
       (when-let [m1 mat1]
         (when-let [m2 mat2]
          (cons (pmap #(dotProduct (first m1) %) (matTranspose m2)) (matMultipication_lazy (rest m1) m2)) 
      )
     )
    )
   )
  )
 

(defn matPow [mat p]
  {:pre [(sameColsRows? mat mat)] }
    (if (= p 1) 
      mat
    
      (if (even? p)
        (let [rslt (matPow mat (/ p 2))] 
          (matMultipication rslt rslt)
         )
      
        (matMultipication (matPow mat (- p 1)) mat)   
    )  
   )
  )


(defn exec
  ([f m] m)
  ([f m1 m2] (f m1 m2))
  ([f m1 m2 & more] (reduce f (f m1 m2) more))
 )


(defn printMatrix [mat]
 {:pre [(matrix? mat)] }
  (if (empty? mat)
    (print "\n")
    
    (let [ [firstRow & rest] mat] 
      (apply println firstRow)
      (printMatrix (vec rest))
     ) 
  )
 )



(defn equalToTranspose [mat s]
  {:pre [(matrix? mat) (isSquare? mat) (or (= 1 s) (= -1 s))] }
  (let [transMat (matScalarMul (matTranspose mat) s)]
     (let [result (pmap #(apply = true (map = %1 %2)) mat transMat)]
        (apply = true result)
       )
     )
  )


(defn isSymmetric [mat]
  {:pre [(matrix? mat) (isSquare? mat)] }
  (equalToTranspose mat 1)
  )

(defn isAntiSymmetric [mat]
  {:pre [(matrix? mat) (isSquare? mat)] }
  (equalToTranspose mat -1)
  )



(defn matTrace [mat]
  {:pre [(matrix? mat) (isSquare? mat)]}
  
  (let [size (count mat)]
    (loop [trace 0 m1 mat i 0]
       (if (= i size)
         trace
      
       (recur (+ trace (nth (first m1) i)) (rest m1) (inc i) )
      
        )      
      )
    )
  )


(defn vecDeleteAt [n v1]
  {:pre [(vector? v1)] }
  
  (vec (concat (take n v1) (drop (inc n) v1)))
  
  )

(defn matDeleteCol [n mat]
  {:pre [(matrix? mat)] }
  
  (vec (pmap #(vecDeleteAt n %) mat))
 )


(defn createSignVec [n]
  
  (loop [v (vector) i 0]
    (if (= i n)
       v
    
      (recur (conj v (if (even? i) 1 -1)) (inc i))
      )  
    )
  )


(defn matDet [mat]
  {:pre [(matrix? mat) (isSquare? mat)] }
  (if (or (empty? mat) (empty? (first mat)))
    1
    
   (let [signVec (createSignVec (count (first mat)))]
    (let [firstRow (multipicationVec signVec (first mat))]
      (let [index (range (count firstRow))]
     
       (apply +  
            (map 
              #(* (matDet (rest (matDeleteCol %2 mat))) %1)
            
              firstRow index)
           )     
         )
       )
     )
   )
 )


(defn isInvertible [mat]
  {:pre (matrix? mat)}
  
  (and
    (isSquare? mat)
    (not (zero? (matDet mat)))
   )
 )
 

  
