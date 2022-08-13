#lang racket
(require "interpreterv2.0.0.rkt")
(require "simpleParser.rkt")

; Test 0: This code shoulf return 100.
;(interpret "SampleCode1.txt")

; Test 1: This code should return 150.
(interpret "Tests/Test_1.txt")

; Test 2: This code should return -4.
(interpret "Tests/Test_2.txt")

; Test 3: This code should return 10.
(interpret "Tests/Test_3.txt")

; Test 4: This code should return 16.
(interpret "Tests/Test_4.txt")

; Test 5: This code should return 220.
(interpret "Tests/Test_5.txt")

; Test 6: This code should return 5.
(interpret "Tests/Test_6.txt")

; Test 7: This code should return 6.
(interpret "Tests/Test_7.txt")

; Test 8: This code should return 10.
(interpret "Tests/Test_8.txt")

; Test 9: This code should return 5.
(interpret "Tests/Test_9.txt")

; Test 10: This code should return -39.
(interpret "Tests/Test_10.txt")

; Test 11: This code should give an error (using before declaring).
;(interpret "Tests/Test_11.txt")

; Test 12: This code should give an error (using before declaring).
;(interpret "Tests/Test_12.txt")

; Test 13: This code should give an error (using before assigning).
;(interpret "Tests/Test_13.txt")

; Test 14: This code should give an error (redefining). This is not a required error, but it would be nice if you could catch these.
;(interpret "Tests/Test_14.txt")

; Test 15: This code should return true (not #t).
(interpret "Tests/Test_15.txt")

; Test 16: This code should return 100.
(interpret "Tests/Test_16.txt")

; Test 17: This code should return false (not #f).
(interpret "Tests/Test_17.txt")

; Test 18: This code should return true.
(interpret "Tests/Test_18.txt")

; Test 19: This code should return 128.
(interpret "Tests/Test_19.txt")

; Test 20: This code should return 12;
(interpret "Tests/Test_20.txt")

; Test 21: This code should return 30.
(interpret "Tests/Test_21.txt")

; Test 22: This code should return 11.
(interpret "Tests/Test_22.txt")

; Test 23: This code should return 1106.
(interpret "Tests/Test_23.txt")

; Test 24: This code should return 12.
(interpret "Tests/Test_24.txt")

; Test 25: This code should return 16.
(interpret "Tests/Test_25.txt")

; Test 26: This code should return 72.
(interpret "Tests/Test_26.txt")

; Test 27: This code should return 21.
(interpret "Tests/Test_27.txt")

; Test 28: This code should return 164.
(interpret "Tests/Test_28.txt")