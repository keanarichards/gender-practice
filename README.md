# Gender and practice paper 

Keana Richards 

--- 

Note: words that are italicized below represent code   

## Setting up: 
 
To automate the compilation process using the Makefile in the main directory (aka the final product will be created for you without having to manually re-run all of the files), you will need to have GNU Make downloaded, which can be used to run the Makefile that compiles the whole project (or parts of the project as needed). See [Jenny Bryan's incredible course, with one of the more comprehensible explanations of Makefiles I could find](https://stat545.com/automation-overview.html) for more details on makefiles and to find more resources on using makefiles. 
I also recommend reading the section on Makefiles from "Reproducible Research with R and RStudio" from Christopher Gandrud for a beginner-friendly explanation of what they do and some basix syntax. 

Once GNU Make is downloaded, check that it will be used to compile the product by going to the 'Build' >> 'Configure Build Tools' at the top of the screen in RStudio. Under the dropdown for Project build tools, click 'Makefile' and make sure that the "Makefile directory" is set to "(Project Root)." If there are any issues with this, try the link above to learn more about GNU Make or the various resources on that page. 

I use the "here" package, which is installed and loaded automatically in my code (see more on why the "here" package is a good idea [here](http://jenrichmond.rbind.io/post/how-to-use-the-here-package/), [here](https://github.com/jennybc/here_here), and [here](https://malco.io/2018/11/05/why-should-i-use-the-here-package-when-i-m-already-using-projects/) (: 
All of the code should work without needing to change anything, as long as there is a.Rproj file that the here package can find, which it will use as the root directory from which all other paths will start. If for some reason the here package is using the current working directory as the path, use dr_here() to figure out why here chose the path it ended up using and set_here() to manually change the root directory. 

Running the makefile using *make all* in the RStudio Terminal will re-run all code from start to finish to produce the final product. If the terminal is not preferable, another option to compile the final product is to click "Build" at the top of RStudio and run "Build all."

To run specific parts of the analyses, enter *make help* into the terminal, which will list commands and their respective output for some of the major parts of the analyses (e.g., results documents, methods documents) 

## Logistics:

All variables names and descriptions can be found in the files called "vars-and-labels" in respective (e.g., pilot, study 1, or study 2) data directory

Excluded participants for each study can be found in data - "excluded.csv" files in respective (e.g., either pilot, study 1, or study 2) data directory 

## Coding scheme:

Reference categories: 

Gender = men <br><br>
Competition choice = piece-rate 

