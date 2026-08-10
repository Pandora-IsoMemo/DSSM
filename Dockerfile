FROM inwt/r-shiny:4.4.3

ENV CHROMOTE_CHROME=/usr/bin/google-chrome

# Install Chrome manually + dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    fonts-liberation \
    libmagick++-dev \
    libsodium-dev \
		libglpk-dev \
    pandoc \
    jags \
    && wget -O /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get install -y /tmp/google-chrome.deb \
    && rm /tmp/google-chrome.deb \
    && rm -rf /var/lib/apt/lists/*


RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install nimble and ellmer from GitHub
RUN Rscript -e "install.packages('nimble', repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/2025-03-01', version = '1.3.0')" \
    && Rscript -e "remotes::install_github(c('r-lib/httr2@v1.2.3', 'tidyverse/ellmer@v0.4.1'))"

COPY . .

# Install DSSM & ReSources
RUN installPackage ReSources \
    && installPackage

EXPOSE 3838

CMD ["Rscript", "-e", "library(shiny);DSSM::startApplication(3838)"]


