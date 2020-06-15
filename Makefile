.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

all: paper/paper.html

## pilot 

pilot/data/clean.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<

pilot/data/excluded.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<

pilot/data/vars-labels.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<

pilot/source/01_preregistered-analyses.R: pilot/data/clean.csv

pilot/paper_sections/methods.html: pilot/paper_sections/methods.Rmd pilot/data/clean.csv
	Rscript -e 'rmarkdown::render("$<")'
	

pilot/paper_sections/results.html: pilot/paper_sections/methods.Rmd pilot/data/clean.csv
	Rscript -e 'rmarkdown::render("$<")'


## study 1




## study 2 



## paper 


paper/paper.html: paper/paper.rmd pilot/paper_sections/methods.Rmd
	Rscript -e 'rmarkdown::render("$<")'


clean: 
	rm -f words.txt histogram.tsv histogram.png report.md report.html
