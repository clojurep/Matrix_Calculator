(ns tests
  (:use [clojure.test] [Matrix-Calculator])
  
  )

;;; Here are all of our example tests.
;;; You can add any kind of test you would like.
;;; To execute the tests, just press Ctrl+Alt+S



;;; Matrix addition test
(def matA1 [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])

(def matA2 [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])

(def matA1+A2 (matScalarMul matA1 2))

(def matA3 [[3 2 1] [6 5 4] [9 8 7]])

(def matA4 [[1 5 10] [2 4 3] [0 8 5]])

(def matA3+A4 [[4 7 11] [8 9 7] [9 16 12]])
;;;;;;;;;;;;;;;;


;;; Matrix Determinant test. |matD1| = -36
(def matD1 [[1 2 2] [5 6 0] [1 1 9]])
;;;;;;;;;;;;;;;;


;;; Matrix Multipication test.
(def matM1 [[3 2 1] [6 5 4] [9 8 7]])

(def matM2 [[1 5 10] [2 4 3] [0 8 5]])

(def matM1*M2 [[7 31 41] [16 82 95] [25 133 149]])

(def matM3 [[3 7 5] [2 3 4] [4 1 8] [5 7 2]])

(def matM4 [[1 5] [7 4] [2 5]])

(def matM3*M4 [[62 68] [31 42] [27 64] [58 63]])
;;;;;;;;;;;;;;;;


;;; Matrix Invertible test.
(def matI1 [[1 0 0] [0 1 0] [0 0 1]])
(def matNonInver1 [[5 2 3] [6 3 7] [10 4 6]])
;;;;;;;;;;;;;;;;


;;; Matrix Symmetric test.
(def matSym1 [[3 2 1] [2 5 8] [1 8 7]])
;;;;;;;;;;;;;;;;

;;; Matrix Anti-Symmetric test.
(def matAntiSym1 [[0 -1] [1 0]])
;;;;;;;;;;;;;;;;

;;; Matrix Trace test. Trace(matT1) = 7 + 11 + 8 = 26
(def matT1 [[7 12 9] [13 11 15] [20 3 8]])
;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;;;;;;;;;;;;;
;;;;;;;;;;;;;



(deftest test-add
  (are [m1 m2 result]
      (= (matAdd m1 m2) result)
    
      matA1 matA2 matA1+A2
      matA3 matA4 matA3+A4
      "Test the matAdd function"
      )
)

(deftest test-Determinant
  (is 
     (= -38 (matDet matD1))
     "Test Determinant"
   )
 )

(deftest test-multipication 
  (are [op m1 m2 result]
     (= (op m1 m2) result)
     
     matMul matM1 matM2 matM1*M2
     matMul matM3 matM4 matM3*M4
     
     matMul_lazy matM1 matM2 matM1*M2
     matMul_lazy matM3 matM4 matM3*M4     
     "Test both Multipication functions"
   )
 )

(deftest test-conds 
  (are [cond result]
     (cond result)
       
     false? (isInvertible? matNonInver1)
     true? (isInvertible? matI1)
     true? (isSymmetric? matSym1)
     true? (isAntiSymmetric? matAntiSym1)
     
     "Test that condition holds for all the assertions"
   )
 )

(deftest test-Trace
  (is 
     (= 26 (matTrace matT1))
     "Test Trace"
   )
 )

(run-tests)

    
