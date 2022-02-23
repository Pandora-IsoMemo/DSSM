FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage ReSources

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny);MpiIsoApp::startApplication(3838)"]
