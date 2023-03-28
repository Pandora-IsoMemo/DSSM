FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage ReSources \
    && installPackage

CMD ["Rscript", "-e", "library(shiny);MpiIsoApp::startApplication(3838)"]
