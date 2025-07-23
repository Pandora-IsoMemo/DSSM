FROM ghcr.io/pandora-isomemo/base-image:latest

ENV CHROMOTE_CHROME=/usr/bin/google-chrome

ADD . .

# Install Chrome manually + dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    fonts-liberation \
    && wget -O /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get install -y /tmp/google-chrome.deb \
    && rm /tmp/google-chrome.deb \
    && rm -rf /var/lib/apt/lists/*

# Install nimble
RUN Rscript -e "install.packages('nimble', repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/2025-03-01', version = '1.3.0')"

# Install DSSM & ReSources
RUN installPackage ReSources \
    && installPackage

CMD ["Rscript", "-e", "library(shiny);DSSM::startApplication(3838)"]
