(ns Matrix-Calculator)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MATRIX CALCULATOR;;;;
;;;;; FUNCTIONS FILE;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn matrix? 
  "Check if the input represent a matrix"
  [mat]
  
  (and 
      (not (nil? mat))
      (or (vector? mat) (list? mat) (seq? mat))
      (or (apply = true (map vector? mat)) (apply = true (map list? mat)) (apply = true (map seq? mat)))
      (or (empty? mat) (apply = (map #(= (count (first mat)) (count %)) mat)))
    )
  )
 

(defn sameSize? 
  "Returns true if the matrixes have the same size"
  [mat1 mat2]
  
  {:pre [(matrix? mat1) (matrix? mat2)] }
   
   (and
     (= (count mat1) (count mat2))
     (= (count (first mat1)) (count (first mat2)))
    )
  ) 

(defn sameColsRows?
  "Returns true if the number of the rows of the first matrix
    equals to the number of the columnes of the second matrix"
  [mat1 mat2]
  
  {:pre [(matrix? mat1) (matrix? mat2)] }
    
    (or (= (count (first mat1)) (count mat2)) (empty? mat1) (empty? mat2))
  )


(defn matTranspose
  "Calculates the matrix's transpose"
  [mat]
  
  {:pre [(matrix? mat)] }
   
   (vec (apply map vector mat))
 )


(defn isSquare?
  "Returns true if the matrix is a square matrix"
  [mat]
  
  {:pre (matrix? mat) }
  (sameColsRows? mat mat)
  )


(defn sumVector
  "Calculates the sum of the elements of the vector"
  [v]
  
  {:pre [(vector? v)] }
  
   (apply + v)
 )


(defn sumMatrix
  "Calcualtes the sum of the elements of the matrix"
  [mat]
  
  {:pre [(matrix? mat)] }
  
   (apply + (apply concat mat))
  )


(defn createRandVec
  "Creates n-dimensions vector with
      randomized elements within the range"
  [n range]
  
  (loop [v (vector) i 0]
    (if (= i n)
      v
      
     (recur (conj v (rand-int range)) (inc i))
    )
   )
  )


(defn createRandMat [n m range]
  "Creates n X m matrix with randomized
      elements within the range"
  [n m range]
  
  (loop [mat (vector) i 0]
    (if (= i n)
      mat
      
     (recur (conj mat (createRandVec m range)) (inc i))
    )
   )
  ) 
  

(defn addVectors
  "Calculates the sum of two vectors"
  [vec1 vec2]
  
    {:pre [(or (seq? vec1) (vector? vec1))
           (or (vector? vec2) (seq? vec2))
           (= (count vec1) (count vec2)) ] }
   
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

(defn matAdd
  "Calculates the sum of two matrixes"
  [mat1 mat2]
  
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
 

(defn multipicationVec
  "Creates a vector such that each element
    of it is the result of multipication the
      two corresponding elements in vec1 and vec2"
  [vec1 vec2]
  
    {:pre [(vector? vec1) (vector? vec2)
           (= (count vec1) (count vec2)) ]  }
     
     (vec (map * vec1 vec2))
 )

(defn dotProduct
  "Calculates the Dot Product of vec1 and vec2"
  [vec1 vec2]
  
    {:pre [(vector? vec1) (vector? vec2)
           (= (count vec1) (count vec2)) ]  }
    
     (apply + (map * vec1 vec2))
)


(defn mulBy
  "Returns a function that will multiply it argument by s"
  [s]
  
  #(* % s)
 )


(defn vecScalarMul
  "Calculates the Scalar Multipication of v1 by s"
  [v1 s]
  
  {:pre [(vector? v1)] }
  
    (lazy-seq
      (when-let [v1 (seq v1)]
       (pmap (mulBy s) v1)
    
     )
   )
 )

(defn matScalarMul
  "Calculates the Scalar Multipication of mat by s"
  [mat s]
  
  {:pre [(matrix? mat)] }

     (lazy-seq
       (when-let [m1 (seq mat)]
           (pmap #(vecScalarMul % s) m1)
     )
   )
 )


(defn matSub
  "Calculates the subtraction of two matrixes
     by using the addition and scalar multipication functions"
  [mat1 mat2]
  
  {:pre [(sameSize? mat1 mat2)] }
  
   (matAdd mat1 (matScalarMul mat2 -1)) 
 )


(defn matMul
  "Calculates the multipication of two matrixes"
  [mat1 mat2]
  
  {:pre [(matrix? mat1) (matrix? mat2) 
         (sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
      nil
    
     (vec
       (loop [rslt (vector) m1 mat1]
         (if (= (count m1) 0)
            rslt
      
            (recur
              (conj
                rslt
                (vec 
                  (pmap #(dotProduct (first m1) %)
                        (matTranspose mat2))
                 )
               )
              
              (rest m1)
             )
           )      
         )
       )
     )
   )


(defn matMul_lazy
  "Lazy version of matrix multipication function"
  [mat1 mat2]
  
  {:pre [(matrix? mat1) (matrix? mat2) 
         (sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
     nil    
    
     (lazy-seq
       (when-let [m1 mat1]
         (when-let [m2 mat2]
          (cons 
            (pmap #(dotProduct (first m1) %) ; will be the first element
                  (matTranspose m2))
            
            (matMul_lazy (rest m1) m2) ; will be the rest
         )
        )
       )
      )
     )
    )
 

(defn matPow
  "Calculates the p-power of a matrix"
  [mat p]
  
  {:pre [(matrix? mat) (isSquare? mat) (pos? p)] }
    (if (= p 1) 
      mat
    
      (if (even? p)
        (let [rslt (matPow mat (/ p 2))]  ; then
          (matMul rslt rslt)
          )
      
        (matMul (matPow mat (- p 1)) mat) ; else
    )  
   )
  )


(defn exec
  "High-order function that gets a function
    which operates on matrixes, and execute it for
     any number of matrixes" 
  ([f m] m)
  ([f m1 m2] (f m1 m2))
  ([f m1 m2 & more] (reduce f (f m1 m2) more))
 )


(defn matPrint
  "Prints the matrix to the REPL"
  [mat]
  
 {:pre [(matrix? mat)] }
  (if (empty? mat)
    (print "\n")
    
    (let [ [firstRow & rest] mat] 
      (apply println firstRow)
      (matPrint (vec rest))
     ) 
  )
 )


(defn equalToTranspose
  "Returns true if the matrix equals to the transpose,
     with sign (1/-1) parameter of the transpose"
  [mat s]
  
  {:pre [(matrix? mat) (isSquare? mat) (or (= 1 s) (= -1 s))] }
  (let [transMat (matScalarMul (matTranspose mat) s)]
     (let [result (pmap #(apply = true (map = %1 %2)) mat transMat)]
        (apply = true result)
       )
     )
  )


(defn isSymmetric?
  "Returns true if the matrix is symmetric"
  [mat]
  
  {:pre [(matrix? mat) (isSquare? mat)] }
  (equalToTranspose mat 1)
  )


(defn isAntiSymmetric?
  "Returns true if the matrix is anti-symmetric"
  [mat]
  
  {:pre [(matrix? mat) (isSquare? mat)] }
  (equalToTranspose mat -1)
  )


(defn matTrace
  "Calculates the matrix's trace"
  [mat]
  
  {:pre [(matrix? mat) (isSquare? mat)]}
  
  (let [size (count mat)]
    (loop [trace 0 m1 mat i 0]
       (if (= i size)
         trace
      
       (recur 
         (+ trace (nth (first m1) i))
         (rest m1)
         (inc i) 
          )
        )      
      )
    )
  )


(defn vecDeleteAt
  "Delete the n-th element of the vector"
  [n v1]
  
  {:pre [(vector? v1)] }
  
  (vec (concat (take n v1) (drop (inc n) v1)))
  
  )

(defn matDeleteCol
  "Delete the n-th column of the matrix"
  [n mat]
  
  {:pre [(matrix? mat)] }
  
  (vec (pmap #(vecDeleteAt n %) mat))
 )


(defn createSignVec
  "Creates a 'sign vector' for the determinant function.
    It first element is 1, the second is -1, the third is 1
      and so on"
  [n]
  
  (loop [v (vector) i 0]
    (if (= i n)
       v
    
      (recur (conj v (if (even? i) 1 -1)) (inc i))
      )  
    )
  )


(defn matDet
  "Calculates the determinant of a matrix"
  [mat]
  
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


(defn isInvertible?
  "Returns true if the matrix is invertible"
  [mat]
  
  {:pre (matrix? mat)}
  
  (and
    (isSquare? mat)
    (not (zero? (matDet mat)))
   )
 )
 

  
