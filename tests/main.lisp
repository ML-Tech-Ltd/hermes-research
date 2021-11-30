(in-package :cl-user)
(defpackage hermes-research/tests/main
  (:use :cl
        :hermes-research
        :rove))
(in-package :hermes-research/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :api-mlfx)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
           (ok (= 1 1))))
