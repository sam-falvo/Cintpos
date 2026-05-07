GET "mcpl.h"

FUN f
: 0, ?, a => a
: n, b, a => f(n-1, a+b, b)
.
FUN fib
: n => f(n, 1, 0)
.
FUN try : n => writef("fib %2d = %d\n", n, fib n)
.
FUN start : =>
  try 0
  try 1
  try 2
  try 3
  try 4
  try 5
  try 6
  try 7
  try 8
  try 9
  try 10
  RETURN 0
.
