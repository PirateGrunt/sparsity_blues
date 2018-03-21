all:index.html

data_files = nfl_data
data_rda = $(addprefix data/, $(addsuffix .rda, $(data_files)))

index.html:index.Rmd ./css/revealOpts.css $(data_rda)
	Rscript -e "rmarkdown::render('$<')"
	Rscript -e "knitr::purl('$<', documentation = 0)"

data/%.rda:%.R
	Rscript -e "source('$<')"

clean:
	rm -rf data/*.rda
	rm index.html
