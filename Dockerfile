FROM ghcr.io/pandora-isomemo/base-image:latest

RUN adduser --system --disabled-password --home /home/inwt inwt
ENV HOME /home/inwt 
USER inwt

ADD . .

RUN installPackage ReSources \
    && installPackage

CMD ["Rscript", "-e", "library(shiny);MpiIsoApp::startApplication(3838)"]
