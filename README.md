# ShinyMC
### R Shiny based interface for monitoring bioreactors

ShinyMC is a small App for monitoring bioreactor experiments. Its purpose is to keep track of ongoing cultivations in MC-1000-OD bioreactors from Photon System Intruments (PSI, CZ). Other sensors can be connected to ShinyMC as well via import of text-based measurement files. Currently, ShinyMC supports the following features:

- Display of optical density (OD at 680 and 720 nm wavelength)
- Display of growth rate calculated from OD, based on either interval growth rates (batch and continuous cultivation) or frequency of dilutions (continuous)
- Display of temperature, light intensity, and other sensor measurements such as CO2
- All charts are interactive R Shiny modules and can be adjusted by sliders, check boxes and many other parameters

### Structure

ShinyMC consists of a set of R scripts that determine the functionality.

- app.R contains the main body of functions. It is devided into a GUI and a server part. The GUI contains the interactive modules such as sliders and check boxes. The server obtains input parameters from the GUI and adjusts the graphical output accordingly (changes the charts on the fly)
- readSQLite.R reads the SQLite database, formats the data, renames bioreactor channels and writes a csv file that can be imported by other platforms such as spreadsheet applications
- intervalMu.R contains a function to calculate the growth rate from the interval between two successive dilutions. It finds local minima and maxima and fits a linear equation to the data, whose slope is the specific growth rate µ

### Input data

- ShinyMC uses *.csv tables that are imported as data.frames. Valid databases are named according to the scheme 'YYYYMMDD_MCX_measurements.csv'
- It also imports CO2 sensor measurements named 'YYYYMMDD_MCX_CO2.txt'

### Getting started

To start ShinyMC, you need to have R (optionally also Rstudio) and some of its libraries installed, mainly:

- shiny
- RSQLite
- lattice
- latticeExtra
- tidyr
- plyr

Open RStudio, change your working directory to a '.../multicultivator/ShinyMC' path and push the 'Run App' button

OR

Even simpler and for Linux users only, double click the 'ShinyMC' launcher in the /ShinyMC folder. If you want to see the results on the transfer server, double click 'ShinyMC-server'. 

