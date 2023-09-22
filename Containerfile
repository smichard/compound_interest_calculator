FROM rocker/shiny:latest

RUN R -e "install.packages(c('dplyr', 'ggplot2', 'lubridate', 'tibble'), repos='https://cloud.r-project.org/')"

COPY ./shiny_app/* /srv/shiny-server/app/

RUN chown -R shiny:shiny /srv/shiny-server/

EXPOSE 3838

CMD ["/opt/shiny-server/bin/shiny-server"]
