FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage ReSources \
    && installPackage

CMD ["Rscript", "-e", "library(shiny);print(API_BASE_URL);MpiIsoApp::startApplication(3838)"]
