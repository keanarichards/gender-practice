# Gender and practice studies 

Keana Richards 

--- 

## Description of each study

Each study has an associated folder, which contains the data, write up for that specific study, source code, and figures associated with the study. This compartmentalization streamlines the coding process.  

- Study 1 (MTurk) - dissertation study 1.1: effects of knowledge of preparation for multiplication task (vs. control condition where they are not told about opportunity to practice) on choice to compete

- Study 2 (MTurk) - dissertation study 1.2: effects of limited preparation for multiplication task (vs. control condition with filler counting zeros task) on choice to compete

- Study 3 (SONA): effects of limited preparation on confidence and performance on multiplication task 

- Study 4 (MTurk) - dissertation study 1.3: effects of limited preparation for multiplication task (vs. control condition with filler counting zeros task) on choice to compete

- Study 5 (MTurk) - dissertation study 2.1: effects of forced competition (vs. control under piece rate payment) on choice to practice 

## Organization of folders

### Specific study folders 

Each "study" (e.g., study1, study2) folder has the following structure: 

- data folder contains all data associated with the study, including: 
    - "clean.csv" which are the data used for analyses
    - "excluded.csv" which are the data of the ppts that were excluded based on exclusion criteria listed in pre-registration
    - "raw.csv" which are the original dataset downloaded directly from qualtrics
    - "var-and-labels.csv" which contains the variable names & descriptions provided by qualtrics 

- figs folder: all figures produced by plots code stored as .png files

- paper_sections: all paper sections (e.g., methods, results, etc) associated using the specific data in said study

- source folder: all code used to clean and perform confirmatory & exploratory analysis the data, along with other code used to create stimuli etc.  


### Other relevant folders 

- "paper" includes docs necessary to compile APA version of dissertation chapter 1 (submitted to ICA)

- "oxforddown-master" includes docs to compile dissertation version of paper 

- "nsf-application" includes all docs for NSF DDIG submission

- "presentations" holds all presentations using these data 


Note: words that are italicized below represent code   

## Automating building (note: may be outdated): 
 
To automate the compilation process using the Makefile in the main directory (aka the final product will be created for you without having to manually re-run all of the files), you will need to have GNU Make downloaded, which can be used to run the Makefile that compiles the whole project (or parts of the project as needed). See [Jenny Bryan's incredible course, with one of the more comprehensible explanations of Makefiles I could find](https://stat545.com/automation-overview.html) for more details on makefiles and to find more resources on using makefiles. 
I also recommend reading the section on Makefiles from "Reproducible Research with R and RStudio" from Christopher Gandrud for a beginner-friendly explanation of what they do and some basix syntax. 

Once GNU Make is downloaded, check that it will be used to compile the product by going to the 'Build' >> 'Configure Build Tools' at the top of the screen in RStudio. Under the dropdown for Project build tools, click 'Makefile' and make sure that the "Makefile directory" is set to "(Project Root)." If there are any issues with this, try the link above to learn more about GNU Make or the various resources on that page. 

I use the "here" package, which is installed and loaded automatically in my code (see more on why the "here" package is a good idea [here](http://jenrichmond.rbind.io/post/how-to-use-the-here-package/), [here](https://github.com/jennybc/here_here), and [here](https://malco.io/2018/11/05/why-should-i-use-the-here-package-when-i-m-already-using-projects/) (: 
All of the code should work without needing to change anything, as long as there is a.Rproj file that the here package can find, which it will use as the root directory from which all other paths will start. If for some reason the here package is using the current working directory as the path, use dr_here() to figure out why here chose the path it ended up using and set_here() to manually change the root directory. 

Running the makefile using *make all* in the RStudio Terminal will re-run all code from start to finish to produce the final product. If the terminal is not preferable, another option to compile the final product is to click "Build" at the top of RStudio and run "Build all."

To run specific parts of the analyses, enter *make help* into the terminal, which will list commands and their respective output for some of the major parts of the analyses (e.g., results documents, methods documents) 

## Coding scheme:

Reference categories: 

Gender = men <br><br>
Competition choice = piece-rate 


## Some notes on the dissertation using oxforddown 

- Since my original root folder is gender-practice before I decided to start using oxforddown-master for the dissertation, needed a workaround for the fact that knitting the dissertation relies on having oxforddown-master as the root directory, which is not how the studies in gender-practice are set up. To avoid transferring everything to oxforddown-master (which would mess up all of the dependencies in the source folders, requiring relabeling of many files), I will using temporarily set the root directory to gender-practice by using here_i_am() from here package (based on idea [here](https://stackoverflow.com/questions/51712725/use-the-here-function-to-go-up-a-level-above-root-directory)), which allows me to go up to the gender-practice root directory and pull the associated docs etc from there when I am knitting individual chapters.  

