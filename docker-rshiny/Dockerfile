# Get shiny server with tidyverse installed already
FROM rocker/shiny-verse:3.6.1

# Install a few dependencies for packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  && install2.r --error \
    plotly \
    RPostgreSQL \
    pool \
    RColorBrewer \
    shinythemes \
    shinycssloaders \
    viridis

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN chmod +xr /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
