FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

# Install Chrome manually + dependencies
RUN apt-get update && apt-get install -y \
    wget \
    gnupg \
    ca-certificates \
    fonts-liberation \
    libnss3 \
    libxss1 \
    libasound2 \
    libatk-bridge2.0-0 \
    libgtk-3-0 \
    && wget -O /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get install -y /tmp/google-chrome.deb \
    && rm /tmp/google-chrome.deb \
    && rm -rf /var/lib/apt/lists/*

ENV CHROMOTE_CHROME=/usr/bin/google-chrome

RUN installPackage ReSources \
    && installPackage

CMD ["Rscript", "-e", "library(shiny);DSSM::startApplication(3838)"]
