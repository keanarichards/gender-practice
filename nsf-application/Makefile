all: 00-project_summary.docx 01-project_description.docx 02-budget_justification.docx 03-facilities.docx 04-support.docx 05-bio_sketch.docx 06-data_mgt_plan.docx


.PHONY: all clean help
.DELETE_ON_ERROR:
.SECONDARY:

help: Makefile
	@sed -n 's/^##//p' $<
	
	
## project summary: 

00-project_summary.docx: 00-project_summary.Rmd
	Rscript -e 'rmarkdown::render("$<")'

## project description: 

01-project_description.docx: 01-project_description.Rmd
	Rscript -e 'rmarkdown::render("$<")'

## budget justification

02-budget_justification.docx: 02-budget_justification.Rmd
	Rscript -e 'rmarkdown::render("$<")'


## facilities, equipment, and other resources

03-facilities.docx: 03-facilities.Rmd
	Rscript -e 'rmarkdown::render("$<")'

## current and pending support: 

04-support.docx: 04-support.Rmd
	Rscript -e 'rmarkdown::render("$<")'


## biographical sketch: 

05-bio_sketch.docx: 05-bio_sketch.Rmd
	Rscript -e 'rmarkdown::render("$<")'

## data management plan:  


06-data_mgt_plan.docx: 06-data_mgt_plan.Rmd
	Rscript -e 'rmarkdown::render("$<")'

## main doc that includes all references in one place (project summary, project description, and facilities docs)

0X-parent_doc.docx: 0X-parent_doc.Rmd
	Rscript -e 'rmarkdown::render("$<")'


clean: 
	rm -f .Rhistory
