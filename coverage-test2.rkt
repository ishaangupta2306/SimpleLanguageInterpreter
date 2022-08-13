#lang racket

(require "interpreterv2.0.0.rkt")
(require "simpleParser.rkt")

;Test 1: This code should return 20.
(parser "Tests2/Test_1.txt")

; Test 2: This code should return 164.
(interpret "Tests2/Test_2.txt")

; Test 3: This code should return 32.
(interpret "Tests2/Test_3.txt")

; Test 4: This code should return 2.
(interpret "Tests2/Test_4.txt")

; Test 5: This code should give an error.
(interpret "Tests2/Test_5.txt")

; Test 6: This code should return 25.
(interpret "Tests2/Test_6.txt")

; Test 7: This code should return 21.
(interpret "Tests2/Test_7.txt")

; Test 8: This code should return 6.
(interpret "Tests2/Test_8.txt")

; Test 9: This code should return -1.
(interpret "Tests2/Test_9.txt")

; Test 10: This code should return 789.
(interpret "Tests2/Test_10.txt")

; Test 11: This code should give an error.
(interpret "Tests2/Test_11.txt")

; Test 12: This code should give an error.
(interpret "Tests2/Test_12.txt")

; Test 13: This code should give an error.
(interpret "Tests2/Test_13.txt")

; Test 14: This code should return 12.
(interpret "Tests2/Test_14.txt")

; Test 15 should return 125.
(parser "Tests2/Test_15.txt")

; Test 16 should return 110.
(interpret "Tests2/Test_16.txt")

; Test 17 should return 2000400.
(interpret "Tests2/Test_17.txt")

; Test 18 should return 101.
(interpret "Tests2/Test_18.txt")

; Test 19 should give an error.
(interpret "Tests2/Test_19.txt")

; Additional Test for Those Seeking an Extra Challenge
; Test 20: This code should return 21.
(interpret "Tests2/Test_20.txt")