
(ert-deftest decl--test--decl--property-list-put-and-keep ()
  "Tests the function DECL--PROPERTY-LIST-PUT-AND-KEEP"
  (let ((temp nil))
    (decl--property-list-put-and-keep temp :test 5)
    (should (eq 5 (plist-get temp :test)))
    (decl--property-list-put-and-keep temp :test 7)
    (should (eq 7 (plist-get temp :test)))
    (decl--property-list-put-and-keep temp :other 3)
    (should (eq 3 (plist-get temp :other)))))

(ert-deftest decl--test--decl--list-cons-and-keep ()
  "Tests the function DECL--LIST-CONS-AND-KEEP"
  (let ((temp nil))
    (should (equal (car (decl--list-cons-and-keep temp 6)) 6))
    (decl--list-cons-and-keep temp "test")
    (should (equal (car temp) "test"))
    (decl--list-cons-and-keep temp :test)
    (should (eq (car temp) :test))
    (decl--list-cons-and-keep temp 6)
    (should (eq (car temp) 6))
    (should (eq (length temp) 4))))

(ert-deftest decl--test--decl--string-concat-and-keep ()
  "Tests the function DECL--STRING-CONCAT-AND-KEEP"
  (let ((a "test")
        (b "other"))
    (decl--string-concat-and-keep a b)
    (should (equal "testother" a))
    (should (equal "othertestother" (decl--string-concat-and-keep b a)))))

(ert-deftest decl--test--decl--every-keywordp ()
  "Tests the function DECL--EVERY-KEYWORDP"
  (should (eq t (decl--every-keywordp '(:te :test :a :b :e))))
  (should (eq nil (decl--every-keywordp '(nil :test :a))))
  (should (eq nil (decl--every-keywordp '(defun :te))))
  (should (eq nil (decl--every-keywordp '(5)))))

(ert-deftest decl--test--decl--generate-increasing-number-string ()
  "Tests the function DECL--GENERATE-INCREASING-NUMBER-STRING"
  (let ((decl--increasing-count 0))
    (should (equal "1" (decl--generate-increasing-number-string)))
    (dotimes (i 100) (decl--generate-increasing-number-string))
    (should (equal "102" (decl--generate-increasing-number-string)))))

(ert-deftest decl--test--decl--property-list-keys () ; Relies on decl--property-list-put-and-keep
  "Tests the function DECL--PROPERTY-LIST-KEYS"
  (let ((test-plist nil))
    (decl--property-list-put-and-keep test-plist :other 78)
    (should (equal '(:other) (decl--property-list-keys test-plist)))
    (decl--property-list-put-and-keep test-plist :test 8)
    (should (equal '(:test :other) (decl--property-list-keys test-plist)))))

(ert-deftest decl--test--decl-compare-plist-based-digraph-consisting-of-eq-able-elements ()
  "Tests the function DECL---COMPARE-PLIST-BASED-DIGRAPH-CONSISTING-OF-EQ-ABLE-ELEMENTS"
  (should (eq (decl---compare-plist-based-digraph-consisting-of-eq-able-elements
               '(:a (:b) :b (:a)) '(:b (:a) :a (:b))) t))
  (should (eq (decl---compare-plist-based-digraph-consisting-of-eq-able-elements
               '(:a (:b) :b (:a)) '(:b (:a) :a (2))) nil)))

(ert-deftest decl--test--decl-tarjan-strongly-connected-components-algorithm-for-plist-based-digraphs-consisting-of-eq-able-elements
    () "Tests the function decl--test--decl-tarjan-strongly-connected-components-algorithm-for-plist-based-digraphs-consisting-of-eq-able-elements

WARNING: This test is not robust! Failure of this test does not necessarily imply failure of the function this is testing! Comparison function is not adequate.
WARNING 2: Only 'visually' verified!"
    (should (eq
             (tree-equal
              (decl--tarjan-strongly-connected-components-algorithm-for-plist-based-digraph-consisting-of-eq-able-elements
               '(1 (2) 2 (3) 3 (4) 4 nil))
              '(((:value 1 :index 3 :lowlink 3)) ((:value 2 :index 2 :lowlink 2)) ((:value 3 :index 1 :lowlink 1)) ((:value 4 :index 0 :lowlink 0))))
             t))
    (should (eq
             (tree-equal
              (decl--tarjan-strongly-connected-components-algorithm-for-plist-based-digraph-consisting-of-eq-able-elements
               '(1 (2 3) 2 (1) 3 (1) 4 (5) 5 nil))
              '(((:value 3 :index 2 :lowlink 2)) ((:value 1 :index 2 :lowlink 2) (:value 2 :index 4 :lowlink 3)) ((:value 4 :index 1 :lowlink 1)) ((:value 5 :index 0 :lowlink 0)))
              )
             t))
    (should 
     (eq
      (tree-equal
       (decl--tarjan-strongly-connected-components-algorithm-for-plist-based-digraph-consisting-of-eq-able-elements
        '(:a (:b :c) :b (:a) :c (:a) :d (:e) :e nil))
       '(((:value :c :index 2 :lowlink 2)) ((:value :a :index 2 :lowlink 2) (:value :b :index 4 :lowlink 3)) ((:value :d :index 1 :lowlink 1)) ((:value :e :index 0 :lowlink 0))))
      t)))

