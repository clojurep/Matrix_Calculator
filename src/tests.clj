(ns tests
  (:use [clojure.test] [Matrix-Calculator])
  
  )
;;;
(def mat11 [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])

(def mat12 [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])

(def mat11+12 (matScalarMul mat11 2))

(def mat4 [[1 2 2] [5 6 0] [1 1 9]])
;;;;

(def matI [[1 0 0] [0 1 0] [0 0 1]])

;;;;

(def matNonInver [[5 2 3] [6 3 7] [10 4 6]])

;;;;

(def largeMat (createRandMat 7 7 20))

;;;;

(def mat2 [[3 2 1] [6 5 4] [9 8 7]])

(def mat3 [[1 5 10] [2 4 3] [0 8 5]])

(def matSym [[3 2 1] [2 5 8] [1 8 7]])

(def matAntiSym [[0 -1] [1 0]])

(def mat2+3 [[4 7 11] [8 9 7] [9 16 12]])

(def mat2*3 [[7 31 41] [16 82 95] [25 133 149]])
;;;

(deftest check-add
  (are [m1 m2 result]
      (= (matAdd m1 m2) result)
    
      mat2 mat3 mat2+3
      mat11 mat12 mat11+12
      "Test the addMatrixes function"
      )
)

(deftest check-multipication 
  (is 
     (= (matMul mat2 mat3) mat2*3)
     "Check Multipication for an example"
   )
 )

(deftest check-invertible 
  (is 
     (false? (isInvertible matNonInver))
     "Check inverse"
   )
 )

(deftest check-Determinant
  (is 
     (= -38 (matDet mat4))
     "Check Determinant"
   )
 )

(run-tests)

    
