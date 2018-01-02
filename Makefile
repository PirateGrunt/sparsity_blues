all:pres.html

pres.html:pres.Rmd data/nfl_data.rda
	Rscript -e "rmarkdown::render('$<')"

data/nfl_data.rda:nfl_data.R
	Rscript -e "source('nfl_data.R')"

clean:
	rm -rf data/*.rda
	rm pres.html
