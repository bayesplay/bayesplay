## Resubmission after archive

Fixed warning on undocumented s3 methods


## Test environments

* R-hub builder macOS R-release
* R-hub builder Debian Linux, R-release, GCC
* R-hub builder Fedora Linux, R-devel, clang, gfortran
* R win-builder R-devel 

## R CMD check results

1 NOTE
New submission

Package was archived on CRAN because of warning on undocumented S3 method

## Update

Minor bug-fix to `make_likelihood.noncentral_d2()`

## Resubmission 

This is a resubmission. The following issues have be addressed
* Added missing \value Rd-tags to cash-bayesplay-method.Rd, 
names-bayesplay-method.Rd, plot.Rd, summary-bf-method.Rd
* Removed printing to console except through summary and show methods

## Resubmission

Fix license to match CRAN template

## Test environments

* local OS X install, R 4.0.2
* local OS X install, R 4.1.0
* local alpine linux (3.13.5), R 4.0.5
* local alpine linux (3.13.5), R 4.1.0
* R-hub builder Windows Server 2008 R2 SP1, R-devel
* R-hub builder Ubuntu Linux 20.04, R-release
* R win-builder R 4.0.5 



## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs

