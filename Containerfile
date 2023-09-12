# Verwenden Sie das offizielle Shiny-Server-Image als Basis
FROM rocker/shiny:latest

# Installieren Sie zusätzliche R-Pakete, falls benötigt (z.B. dplyr, ggplot2)
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'lubridate', 'tibble'), repos='https://cloud.r-project.org/')"

# Kopieren Sie die Shiny-App-Dateien in den Container
# Hier wird angenommen, dass Ihre App-Dateien (ui.R und server.R) im aktuellen Verzeichnis liegen
COPY /path-to-your-shiny-app/ /srv/shiny-server/myapp/

# Ändern Sie die Berechtigungen, damit Shiny-Server darauf zugreifen kann
RUN chown -R shiny:shiny /srv/shiny-server/myapp

# Exponieren Sie den Port 3838, auf dem Shiny-Server läuft
EXPOSE 3838

# Starten Sie den Shiny-Server
CMD ["/usr/bin/shiny-server.sh"]
