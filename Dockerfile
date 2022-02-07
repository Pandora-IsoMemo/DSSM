FROM resources-isoapp-base:latest

ENV PKG MpiIsoApp

RUN Rscript -e "install.packages('ReSources')"

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny);MpiIsoApp::startApplication(3838)"]
