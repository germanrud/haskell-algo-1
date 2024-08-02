import MisFunciones 
import Test.HUnit

testFibo = test [
    " "  ~: (fib 0) ~?= 0,
    " f" ~: (fib 1) ~?= 1 ]
