# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.3

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /worldheritagewikipedia

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev libcairo2-dev libproj-dev -y \
  # build this compendium package
  && R -e "devtools::install('/worldheritagewikipedia', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/worldheritagewikipedia/analysis/paper/paper.Rmd')"
