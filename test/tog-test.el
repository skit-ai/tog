(require 'tog-utils)

(describe "Upsert"
  (it "works"
    (expect (upsert 5 #'zerop '(1 2 3)) :to-equal '(5 1 2 3))
    (expect (upsert 5 #'zerop '(0 2 3)) :to-equal '(5 2 3))))
