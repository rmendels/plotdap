## Test environments
* local OS X install, R 3.5.3
* Fedora Linux, R-devel, clang, gfortran (on RHub)
* win-builder (devel and release)

## R CMD check results

### OS X

Duration: 8m 35.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
   
### Winbuilder release
New submission

Possibly mis-spelled words in DESCRIPTION:
  rerddap (9:81)
  
 Spelling is correct


### Winbuilder devel

New submission

Possibly mis-spelled words in DESCRIPTION:
  rerddap (9:81)

Spelling is correct

### RHUb Windows Server 2008 R2 SP1, R-devel, 32/64 bit

New submission

Possibly mis-spelled words in DESCRIPTION:
  rerddap (9:81)
  
 Spelling is Correct

### RHub Fedora

New submission

Possibly mis-spelled words in DESCRIPTION:
  rerddap (9:81)
  
Spelling is correct

### Comments

In response to reviewer's comments on last submission, the `rerddap` that
would download the data that is contained in the package datasets have 
been uncommented and put in \donttest{}.  All other requested changes made.

* This is a new release.
