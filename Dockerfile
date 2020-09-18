FROM rocker/shiny
WORKDIR /home/shiny
RUN mkdir /home/shiny/www
COPY www/* /home/shiny/www/
RUN mkdir /home/shiny/data
COPY data/* /home/shiny/data/
COPY installPrerequisites.R .
RUN apt-get update && apt-get install -y \
     libssl-dev
#    libcurl4-openssl-dev 
#    libxml2-dev \
#    libssl-dev \
#    libcairo2-dev \
#    libxt-dev \
#    parallel
RUN R -f ./installPrerequisites.R .
COPY *.R ./
CMD ["R", "-e", "shiny::runApp('/home/shiny/app.R', port=3838, host='0.0.0.0')"]

