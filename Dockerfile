# Use rocker/r-ver as base image with R pre-installed
FROM rocker/r-ver:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    wget \
    unzip \
    default-jdk \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    gdal-bin \
    libcairo2-dev \
    libgdal-dev \
    libglpk-dev \
    libudunits2-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Set JAVA_HOME environment variable
ENV JAVA_HOME=/usr/lib/jvm/default-java

# Compatibility: some tooling/scripts assume this Ubuntu path exists.
RUN ln -sf /usr/lib/jvm/default-java /usr/lib/jvm/java-1.17.0-openjdk-amd64

# Download and install NetLogo 6.2.2
RUN wget -q https://downloads.netlogo.org/6.2.2/NetLogo-6.2.2-64.tgz \
    && tar -xzf NetLogo-6.2.2-64.tgz \
    && mv NetLogo\ 6.2.2 /opt/netlogo \
    && rm NetLogo-6.2.2-64.tgz

# Set NetLogo path
ENV NETLOGO_HOME=/opt/netlogo

# Create working directory
WORKDIR /app

# Install renv and restore packages from the lockfile into the site library.
# The project code is intentionally NOT baked into the image; it should be mounted at runtime.
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

COPY renv.lock /tmp/renv.lock
RUN R -e "renv::restore(lockfile='/tmp/renv.lock', library='/usr/local/lib/R/site-library', prompt=FALSE)"
RUN rm -f /tmp/renv.lock

# Default entrypoint: pass an R script path as the container argument.
ENTRYPOINT ["Rscript"]
CMD ["--help"]