## Test environments
* local OS X install, R 3.5.3
* Fedora Linux, R-devel, clang, gfortran (on RHub)
* win-builder (devel and release)

## R CMD check results

### OS X

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

   Examples with CPU or elapsed time > 5s
            user system elapsed
   plotdap 5.446  0.382    5.84
   ** found \donttest examples: check also with --run-donttest
   
I have done everything possible to cut down example times,  including putting the
necessary data in the package.  plotdap.R actually contains the four main
functions, plotdap(), add_griddap(), add_tabledap(),  add_griddap() so it is
the combined times for the examples for these four functions that is 5.5 seconds.

   
### Winbuilder release

2 notes

Possibly mis-spelled words in DESCRIPTION:
  rerddap (2:60, 8:81)

The Title field should be in title case, current version then in title case:
'Easily Visualize Data from 'ERDDAP' Servers via the rerddap Package'
'Easily Visualize Data from 'ERDDAP' Servers via the Rerddap Package'

rerddap is spelled correcly,  and the first title case is correct since the
package name is "rerddap."

Examples with CPU or elapsed time > 10s
         user system elapsed
plotdap 12.13   0.32   12.51

See comment above about Mac OS X test.  The 12 seconds is the time for all
four of the main functions.

### Winbuilder devel

Status: 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  rerddap (2:60, 8:81)

rerddap is spelled correctly

### Fedora

Possibly mis-spelled words in DESCRIPTION:
  rerddap (2:60, 8:81)

The Title field should be in title case. Current version is:
‘Easily visualize data from 'ERDDAP' Servers via the rerddap package’
In title case that is:
‘Easily Visualize Data from 'ERDDAP' Servers via the Rerddap Package’

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
          user system elapsed
plotdap 15.556  0.348  15.925
** found \donttest examples: check also with --run-donttest

Same comments as above.  Spelling is correct.  Timing is the combined time
for four functions.


* This is a new release.
