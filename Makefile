all:pres.html

data_files = nfl_data
data_rda = $(addprefix data/, $(addsuffix .rda, $(data_files)))

pres.html:pres.Rmd ./css/revealOpts.css data $(data_rda)
	Rscript -e "rmarkdown::render('$<')"
	Rscript -e "knitr::purl('$<', documentation = 0)"

data/%.rda:%.R
	Rscript -e "source('$<')"

clean:
	rm -rf data/*.rda
	rm pres.html
