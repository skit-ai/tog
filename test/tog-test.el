(push default-directory load-path)

(require 'cl-lib)
(require 'ov)
(require 'tog)

(defvar test-file "./test/data/test.tog")

(ert-deftest test-tag-loading ()
  (with-current-buffer (find-file-noselect test-file)
    (should (= (length (cl-remove-if (lambda (o) (null (ov-val o 'tog-line-id))) (ov-all)))
               (with-current-buffer (find-file-noselect (tog-tag-file-name))
                 (count-matches "^[0-9]+: "))))))
