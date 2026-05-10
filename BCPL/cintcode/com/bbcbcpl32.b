// This is a version BCPL to Cintcode compiler modified to
// compile the dialect of BCPL that ran on the BBC Microcomputer.
// Unlike BBC BCPL this version does not equate upper and lower
// case letters in identifiers and reserved words, but it does
// allow reserved words to be in lower or upper case letters.

// This version is based on the bcpl compiler of the modern
// BCPL Cintcode system.

// Implemented by Martin Richards (c) Dec 2019

// Note: ../cintcode/ is used so that the compiler can be
// compiled in a directory parallel to cintcode (such as natbcpl).

GET "../cintcode/com/bbcbcplfe32.b"

.

GET "../cintcode/com/bcplcgcin.b"
