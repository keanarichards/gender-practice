all: paper/draft_paper.docx

.PHONY: all clean help
.DELETE_ON_ERROR:
.SECONDARY:

help: Makefile
	@sed -n 's/^##//p' $<


pilot/data/clean.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<


pilot/data/excluded.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<
	

pilot/data/vars-and-labels.csv: pilot/source/00_load-raw-data-and-clean.R 
	Rscript $<


## pilot/paper_sections/methods.docx: compile results section for pilot study 


pilot/paper_sections/methods.docx: pilot/paper_sections/methods.Rmd pilot/data/clean.csv 
	Rscript -e 'rmarkdown::render("$<")'
	
	
pilot/source/02_plots.R: pilot/data/clean.csv 
	
pilot/figs/%.png: pilot/source/02_plots.R
	Rscript $<
	
## pilot/paper_sections/results.docx: compile results section for pilot study 

pilot/paper_sections/results.docx: pilot/paper_sections/results.Rmd pilot/data/clean.csv \
	pilot/source/01_preregistered-analyses.R pilot/source/03_exploratory-analyses.R \
	pilot/figs/%.png
	Rscript -e 'rmarkdown::render("$<")'


# study 1

study1/data/clean.csv: study1/source/00_load-raw-data-and-clean.R 
	Rscript $<
  
study1/data/excluded.csv: study1/source/00_load-raw-data-and-clean.R 
	Rscript $<
  
study1/data/vars-and-labels.csv: study1/source/00_load-raw-data-and-clean.R 
	Rscript $<

study1/paper_sections/methods.Rmd: study1/data/clean.csv 


## study1/paper_sections/methods.docx: compile results section for study 1


study1/paper_sections/methods.docx: study1/paper_sections/methods.Rmd
	Rscript -e 'rmarkdown::render("$<")'

study1/figs/%.png: study1/source/02_plots.R
	Rscript $<


## study1/paper_sections/results.docx: compile results section for study 1


study1/paper_sections/results.docx: study1/paper_sections/results.Rmd study1/data/clean.csv \
	study1/source/01_preregistered-analyses.R \
	study1/figs/%.png
	Rscript -e 'rmarkdown::render("$<")'



# study 2 

study2/data/clean.csv: study2/source/00_load-raw-data-and-clean.R 
	Rscript $<
  
study2/data/excluded.csv: study2/source/00_load-raw-data-and-clean.R 
	Rscript $<
  
study2/data/vars-and-labels.csv: study2/source/00_load-raw-data-and-clean.R 
	Rscript $<
	
## study2/paper_sections/methods.docx: compile results section for study 2

  
study2/paper_sections/methods.docx: study2/paper_sections/methods.Rmd study2/data/clean.csv 
	Rscript -e 'rmarkdown::render("$<")'

study2/figs/%.png: study2/source/02_plots.R
	Rscript $<

## study2/paper_sections/results.docx: compile results section for study 2

study2/paper_sections/results.docx: study2/paper_sections/results.Rmd study2/data/clean.csv \
	study2/source/01_preregistered-analyses.R \
	study2/figs/%.png
	Rscript -e 'rmarkdown::render("$<")'


# paper 


## paper/pilot.docx: compile section of paper about pilot study 

paper/pilot.docx: paper/pilot.Rmd pilot/paper_sections/methods.docx pilot/paper_sections/results.docx
	Rscript -e 'rmarkdown::render("$<")'

## paper/study1.docx: compile section of paper about study 1 


paper/study1.docx: paper/study1.Rmd study1/paper_sections/methods.docx study1/paper_sections/results.docx
	Rscript -e 'rmarkdown::render("$<")'


## paper/study2.docx: compile section of paper about study 2 


paper/study2.docx: paper/study2.Rmd study2/paper_sections/methods.docx study2/paper_sections/results.docx
	Rscript -e 'rmarkdown::render("$<")'

paper/draft_paper.docx: paper/draft_paper.Rmd paper/pilot.docx paper/study1.docx \
	paper/study2.docx
	Rscript -e 'rmarkdown::render("$<")'

clean: 
	rm -f */*/clean.csv */*/excluded.csv */*/vars-and-labels.csv */*/*.docx */*.docx .Rhistory */*/.Rhistory
	

