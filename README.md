# Gender and practice paper 

Keana Richards 

--- 

How to use and set-up: 

You will need to have GNU Make downloaded, which can be used to run the Makefile that compiles the whole project (or parts of the project as needed). See https://stat545.com/automation-overview.html for more details on makefiles and to find more resources on using makefiles. 

Once GNU Make is downloaded, check that it will be used to compile the product by going to Build >> 'Configure Build Tools.' Under the dropdown for 
Project build tools, click 'Makefile' and make sure that the "Makefile directory" is set to "(Project Root)." 

Then, you must load the here package so the source files will run properly. 
After loading the package, run the following in the console before starting the project:
*set_here(path = '..')*, which will ensure that the gender practice project is the root directory for all associated code. To check that this worked, go to the folder that has the gender-practice.Rproj and make sure that there is a .here file in the same folder as the .Rproj file.  

Running the makefile using *make all* in the RStudio Terminal will re-run all code from start to finish to produce the final product. If the terminal is not preferable, another option to compile the final product is to click "Build" at the top of RStudio and run "Build all."

Logistics:

All variables names and descriptions can be found in the files called "vars-and-labels" in respective data directory

Excluded participants for each study can be found in data - "excluded.csv" files in respective data directory 

## Coding scheme:

Reference categories 

Gender = men
Competition choice = piece-rate 

