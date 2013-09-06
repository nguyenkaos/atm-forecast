atm-forecast
============

Source Code
------------
The main driver of the forecasting component is the file src/main/R/forecast/main.R. 

The layout of the source code follows the Maven standard directory layout.  This should be very famaliar to those in the JVM universe.

* src/main - Source code/configuration for the application
* src/main/R - R source code
* src/main/resources - Required data sets
* src/main/scripts - Scripts to assist running the application
* src/test - Source code/configuration for all tests

Execution
---------
A batch script has been created (src/main/scripts/batchR) that can be used to run an R script in the background.  This script will launch R in batch mode and immediately tail the log file.  You can either add the location of this script to your PATH or simply specify its absolute location.  

For example, to run the setup script using 'batchR' do the following.

```Shell
 cd src/main/R/forecast
 ../../script/batchR setup.R
```

Data Sets
----------

There are 3 different data sets containing the daily ATM usage data.  This is the primary driver of how 
long the application will run.  Each of these data sets vary in size for different purposes.
* usage-all.rds - The entire data set of roughly 18 months of usage across almost 9,000 ATMs. 
* usage-mini.rds - This contains a random sample of roughly 10% of all ATMs.  Each ATM within this data set contains its full 18 month history.
* usage-micro.rds - This contains a random sample of only 3 ATMs.  This data set is useful for sanity checks during development.

Getting Started
---------------

To run the forecast follow these steps.
* Open a UNIX terminal.  Use Cygwin on Windows, if needed.
* Change directory to that containing the forecast code.

```Shell
cd src/main/R/forecast
```

* To ensure that the necessary R packages are installed, run the 'setup.R' script.  This only needs to be done once.

```Shell
../../scripts/batchR setup.R
```

* Execute the main driver for the forecast with the 'micro' data set.

```Shell
../../scripts/batchR main.R
```

* To execute the main driver with a different data set do the following.

```Shell
../../scripts/batchR main.R usage-all.rds
```
