atm-forecast
============

Source Layout
-------------
The layout of the source code follows the Maven standard directory layout.  This should be very
famaliar to those in the JVM universe.

* src/main
  * Source code/configuration for the application
* src/main/R
  * R source code
* src/main/resources
  * Required data sets
* src/main/scripts
  * Scripts to assist running the application
* src/test
  * Source code/configuration for all tests

Getting Started
---------------

There are 3 different data sets containing the daily ATM usage data.  This is the primary driver of how 
long the application will run.  Each of these data sets vary in size for different purposes.
* usage-all.rds - The entire data set of roughly 18 months of usage across almost 9,000 ATMs. 
* usage-mini.rds - This contains a random sample of roughly 10% of the data set.  Each ATM within this data set contains its full 18 month history.
* usage-micro.rds - This contains a random sample of only 3 ATMs.  This data set is useful for sanity checks during development.

run setup

cp withdrawals
$ cd atm-forecast/src/main/R/forecast
$ ../../scripts/batchR driver.R

