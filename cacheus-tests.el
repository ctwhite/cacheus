;;; cacheus-tests.el --- ERT Test Suite for the Cacheus Framework -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This file contains the complete ERT (Emacs Lisp Regression Testing) suite
;; for the `cacheus` library. It is designed to be self-contained and
;; validate all public-facing functionality.
;;
;; To run tests from the command line:
;;   emacs -batch -l ert -l cacheus-tests.el -f ert-run-tests-batch-and-exit
;;
;; To run tests interactively:
;;   M-x ert
;;   Then enter "t" to run all tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dash)

;; Load all cacheus modules to be tested.
(require 'cacheus-core)
(require 'cacheus-cache)
(require 'cacheus-memoize)

;; Concurrency tests are optional.
(require 'concur nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Setup and Teardown

(defvar-local cacheus-test--temp-file nil
  "Path to a temporary file for persistence tests.")

(defun-around-test cacheus-test--cleanup (&rest _)
  "Ensure test isolation by clearing the registry and temp files."
  ;; Clear the global registry before each test run.
  (clrhash cacheus-global-cache-registry)
  (unwind-protect
      ;; Run the actual test body.
      (progn &rest)
    ;; After the test, clean up any temp file and clear the registry again.
    (when (and cacheus-test--temp-file (file-exists-p cacheus-test--temp-file))
      (delete-file cacheus-test--temp-file))
    (setq cacheus-test--temp-file nil)
    (clrhash cacheus-global-cache-registry)))

;; Enable the cleanup hook for all tests defined in this file.
(ert-run-test-hook-enable 'cacheus-test--cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Cache Tests

(ert-deftest cacheus-basic-get-and-put ()
  "Test basic put and get operations."
  (cacheus:cache! test-cache)
  (should (null (cacheus:get! 'test-cache "key1")))
  (should (equal "value1" (cacheus:put! 'test-cache "key1" "value1")))
  (should (equal "value1" (cacheus:get! 'test-cache "key1")))
  (should (= 1 (cacheus:size 'test-cache))))

(ert-deftest cacheus-get-or-compute-miss ()
  "Test that the compute thunk is called on a cache miss."
  (cacheus:cache! test-cache)
  (let ((compute-count 0))
    (should (equal "computed"
                   (cacheus:get! 'test-cache "key1"
                                 (lambda () (cl-incf compute-count) "computed"))))
    (should (= compute-count 1))
    ;; The computed value should now be in the cache.
    (should (equal "computed" (cacheus:get! 'test-cache "key1")))))

(ert-deftest cacheus-get-or-compute-hit ()
  "Test that the compute thunk is NOT called on a cache hit."
  (cacheus:cache! test-cache)
  (let ((compute-count 0))
    (cacheus:put! 'test-cache "key1" "existing")
    (should (equal "existing"
                   (cacheus:get! 'test-cache "key1"
                                 (lambda () (cl-incf compute-count) "computed"))))
    ;; The thunk should not have been called.
    (should (= compute-count 0))))

(ert-deftest cacheus-caching-nil ()
  "Test that `nil' is a valid cacheable value."
  (cacheus:cache! test-cache)
  (cacheus:put! 'test-cache "key1" nil)
  (should (equal 1 (cacheus:size 'test-cache)))
  (should (null (cacheus:get! 'test-cache "key1")))
  ;; A `get!` with a compute thunk should NOT run if `nil` is cached.
  (should (null (cacheus:get! 'test-cache "key1" (lambda () "computed")))))

(ert-deftest cacheus-clear-cache ()
  "Test `cacheus:clear!` functionality."
  (cacheus:cache! test-cache)
  (cacheus:put! 'test-cache "key1" "v1")
  (cacheus:put! 'test-cache "key2" "v2")
  (should (= 2 (cacheus:size 'test-cache)))
  (cacheus:clear! 'test-cache)
  (should (= 0 (cacheus:size 'test-cache)))
  (should (null (cacheus:get! 'test-cache "key1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TTL and Staleness Tests

(ert-deftest cacheus-ttl-expiration ()
  "Test that entries expire after their TTL."
  (cacheus:cache! test-cache :ttl 0.1)
  (let ((compute-count 0))
    (cacheus:get! 'test-cache "key1" (lambda () (cl-incf compute-count) "v1"))
    (should (= compute-count 1))
    ;; Should be a hit immediately after.
    (cacheus:get! 'test-cache "key1" (lambda () (cl-incf compute-count) "v2"))
    (should (= compute-count 1))
    ;; Wait for TTL to expire.
    (sleep-for 0.2)
    ;; Should be a miss now, triggering re-computation.
    (should (equal "v2" (cacheus:get! 'test-cache "key1"
                                      (lambda () (cl-incf compute-count) "v2"))))
    (should (= compute-count 2))))

(ert-deftest cacheus-ttl-refresh-on-access ()
  "Test `:refresh-ttl-on-access` functionality."
  (cacheus:cache! test-cache :ttl 0.2 :refresh-ttl-on-access t)
  (cacheus:put! 'test-cache "key1" "v1")
  (sleep-for 0.15)
  ;; Access the key just before it expires. This should refresh its TTL.
  (should (equal "v1" (cacheus:get! 'test-cache "key1")))
  (sleep-for 0.15)
  ;; The key should still be fresh due to the access.
  (should (equal "v1" (cacheus:get! 'test-cache "key1" (lambda () "expired")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eviction Policy Tests

(ert-deftest cacheus-eviction-lru ()
  "Test Least Recently Used (LRU) eviction policy."
  (cacheus:cache! lru-cache :capacity 3 :eviction-strategy :lru)
  (cacheus:put! 'lru-cache "A" 1) ; A
  (cacheus:put! 'lru-cache "B" 2) ; B A
  (cacheus:put! 'lru-cache "C" 3) ; C B A
  (should (equal '("C" "B" "A") (cacheus:keys 'lru-cache)))
  ;; Accessing "A" should make it the most recently used.
  (cacheus:get! 'lru-cache "A")   ; A C B
  ;; Adding "D" should evict "B".
  (cacheus:put! 'lru-cache "D" 4) ; D A C
  (should (equal '("D" "A" "C") (sort (cacheus:keys 'lru-cache) #'string>)))
  (should (null (cacheus:get! 'lru-cache "B"))))

(ert-deftest cacheus-eviction-fifo ()
  "Test First-In, First-Out (FIFO) eviction policy."
  (cacheus:cache! fifo-cache :capacity 3 :eviction-strategy :fifo)
  (cacheus:put! 'fifo-cache "A" 1)
  (cacheus:put! 'fifo-cache "B" 2)
  (cacheus:put! 'fifo-cache "C" 3)
  ;; Accessing "A" should have no effect on its position for FIFO.
  (cacheus:get! 'fifo-cache "A")
  ;; Adding "D" should evict "A", the first one in.
  (cacheus:put! 'fifo-cache "D" 4)
  (should (equal '("D" "C" "B") (sort (cacheus:keys 'fifo-cache) #'string>)))
  (should (null (cacheus:get! 'fifo-cache "A"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Memoization Tests

(ert-deftest cacheus-memoize-basic ()
  "Test basic function memoization."
  (let ((call-count 0))
    (cacheus:memoize! test-memo-fn (x) (cl-incf call-count) (* x 2))
    (should (= (test-memo-fn 5) 10))
    (should (= call-count 1))
    ;; Call again with same args, should not increment counter.
    (should (= (test-memo-fn 5) 10))
    (should (= call-count 1))
    ;; Call with different args, should increment counter.
    (should (= (test-memo-fn 10) 20))
    (should (= call-count 2))))

(ert-deftest cacheus-memoize-with-keyword-args ()
  "Test memoization of a function with keyword arguments."
  (let ((call-count 0))
    (cacheus:memoize! test-kw-fn (a &key (b 10))
      (cl-incf call-count)
      (+ a b))
    (should (= (test-kw-fn 5) 15))
    (should (= call-count 1))
    (should (= (test-kw-fn 5) 15)) ; Hit
    (should (= call-count 1))
    (should (= (test-kw-fn 5 :b 20) 25)) ; Miss
    (should (= call-count 2))
    (should (= (test-kw-fn 5 :b 20) 25)) ; Hit
    (should (= call-count 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tagging and Invalidation Tests

(ert-deftest cacheus-invalidation-by-tag ()
  "Test invalidating cache entries by tags."
  (cacheus:cache! tag-cache)
  (cacheus:put! 'tag-cache "user-1" "Alice" :tags '(users group-a))
  (cacheus:put! 'tag-cache "user-2" "Bob"   :tags '(users group-b))
  (cacheus:put! 'tag-cache "doc-1"  "Spec"  :tags '(docs group-a))
  (should (= 3 (cacheus:size 'tag-cache)))
  ;; Invalidate all entries with the `group-a` tag.
  (funcall (plist-get (gethash 'tag-cache cacheus-global-cache-registry)
                      :invalidate-by-tags-fn-symbol)
           '(group-a))
  (should (= 1 (cacheus:size 'tag-cache)))
  (should (null (cacheus:get! 'tag-cache "user-1")))
  (should (null (cacheus:get! 'tag-cache "doc-1")))
  (should (equal "Bob" (cacheus:get! 'tag-cache "user-2"))))

(ert-deftest cacheus-invalidation-by-all-tags ()
  "Test invalidating entries with `:all-must-match`."
  (cacheus:cache! tag-cache)
  (cacheus:put! 'tag-cache "A" 1 :tags '(tag1 tag2))
  (cacheus:put! 'tag-cache "B" 2 :tags '(tag1))
  ;; Invalidate entries that have BOTH tag1 AND tag2.
  (funcall (plist-get (gethash 'tag-cache cacheus-global-cache-registry)
                      :invalidate-by-tags-fn-symbol)
           '(tag1 tag2) :all-must-match t)
  (should (null (cacheus:get! 'tag-cache "A")))
  (should (equal 2 (cacheus:get! 'tag-cache "B"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistence Tests

(ert-deftest cacheus-persistence-save-and-load ()
  "Test saving a cache to a file and loading it back."
  (setq cacheus-test--temp-file (make-temp-file "cacheus-test-"))
  (cacheus:cache! persist-cache :cache-file cacheus-test--temp-file)
  (cacheus:put! 'persist-cache "A" 1)
  (cacheus:put! 'persist-cache "B" '(2 "data"))
  ;; Save the cache.
  (cacheus:save-all-caches)
  (should (file-exists-p cacheus-test--temp-file))
  ;; Clear the in-memory cache.
  (cacheus:clear! 'persist-cache)
  (should (= 0 (cacheus:size 'persist-cache)))
  ;; Load the cache from the file.
  (cacheus:load-all-caches)
  (should (= 2 (cacheus:size 'persist-cache)))
  (should (equal 1 (cacheus:get! 'persist-cache "A")))
  (should (equal '(2 "data") (cacheus:get! 'persist-cache "B"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Asynchronous Tests (Conditional)

(when (require 'concur nil t)
  (ert-deftest cacheus-async-get-or-compute ()
    "Test asynchronous cache gets."
    (cacheus:cache! async-cache :async t)
    (let ((promise (cacheus:get! 'async-cache "key" (lambda () (concur:resolved! 42)))))
      (should (concur-promise-p promise))
      (should (= 42 (concur:await! promise))))
    ;; Second get should be a hit, returning a resolved promise.
    (let ((hit-promise (cacheus:get! 'async-cache "key")))
      (should (concur-promise-p hit-promise))
      (should (= 42 (concur:await! hit-promise)))))

  (ert-deftest cacheus-async-thundering-herd-protection ()
    "Test that only one computation runs for concurrent async requests."
    (cacheus:cache! herd-cache :async t)
    (let ((compute-count 0)
          (slow-promise (concur:delay 0.2 (cl-incf compute-count) 123))
          promises)
      ;; Fire off multiple requests for the same key before the first resolves.
      (dotimes (_ 5)
        (push (cacheus:get! 'herd-cache "slow-key" (lambda () slow-promise)) promises))
      ;; Await all promises.
      (let ((results (--map (concur:await! it) promises)))
        ;; Assert that the computation only ran ONCE.
        (should (= 1 compute-count))
        ;; Assert that all callers got the correct result.
        (should (equal results '(123 123 123 123 123))))))
)

(provide 'cacheus-tests)
;;; cacheus-tests.el ends here