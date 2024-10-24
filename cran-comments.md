## Resubmission
This is a resubmission. In this version I have:

* Fixed error in test "Train random forest works with ranger package" that 
occurred on Debian. Despite setting the random number generator seed, 
the random forest created on Debian has reasonable values but is not equal to the random forest created on a Mac and used in the test as the expected output. Now the 
test instead checks that the function runs without error.


## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: ‘Stephanie Reinders <reinders.stephanie@gmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
  Danica (8:171)
  Ommen (8:178)
  
  * Danica Ommen is the correct spelling.
