atm-forecast
============

Getting Started
---------------

The scripts designed to be executable have been configured in a way that allows them to run directly from within a UNIX shell.  Within Windows, a Cygwin terminal will serve the same purpose.  To run the forecast follow these steps.

* To ensure that the necessary R packages are installed, run the 'setup.R' script.  This only needs to be executed once.

```Shell
cd src/main/R/common
./setup.R
```

* Execute the main driver for the forecast.  This defaults to the smallest 'micro' data set which contains data for only 3 ATMs.

```Shell
cd src/main/R/forecast
./main.R
```

* To see all available options use the --help switch.

```Shell
./main.R --help
```

* To execute the main driver with a different data set do the following.  The following example uses the data set containing roughly 8,700 ATMs.

```Shell
./main.R --usageFile=usage-all.rds
```

* To execute the main driver on only a subset of the ATMs do the following.  This is useful for running the forecast across multiple machine in parallel (albeit manually).  The argument must be a valid R expression.  The following example forecasts only the first half of the ATM set.

```Shell
./main.R --atms="atm > median(atm)"
```

Source Code
------------
The main driver of the forecasting component is the file src/main/R/forecast/main.R. 

The layout of the source code follows the Maven standard directory layout.  This should be very famaliar to those in the JVM universe.

* ${ROOT}/src/main - Source code/configuration for the application
* ${ROOT}/src/main/R - R source code
* ${ROOT}/src/main/resources - Required data sets
* ${ROOT}/src/main/scripts - Scripts to assist running the application
* ${ROOT}/src/test - Source code/configuration for all tests

Data Sets
----------

There are 3 different data sets containing the daily ATM usage data.  This is the primary driver of how 
long the application will run.  Each of these data sets vary in size for different purposes.
* usage-all.rds - The entire data set of roughly 18 months of usage across almost 9,000 ATMs. 
* usage-mini.rds - This contains a random sample of roughly 10% of all ATMs.  Each ATM within this data set contains its full 18 month history.
* usage-micro.rds - This contains a random sample of only 3 ATMs.  This data set is useful for sanity checks during development.

