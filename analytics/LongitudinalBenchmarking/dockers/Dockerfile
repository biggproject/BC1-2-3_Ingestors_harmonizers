FROM docker.tech.beegroup-cimne.com/base_dockers/mr-docker:3.10
USER root
RUN apt update

# install R system requirements
RUN apt install -y liblzma-dev  libblas-dev gfortran
RUN apt install -y libreadline-dev
RUN apt install -y build-essential
RUN apt install -y gcc-multilib
RUN apt install -y fort77
RUN apt install -y xorg-dev
RUN apt install -y gobjc++
RUN apt install -y libbz2-dev
RUN apt install -y libpcre2-posix2 libpcre2-dev
RUN apt install -y libcurl4-gnutls-dev
RUN apt install -y make
RUN apt install -y wget
RUN apt install -y libssl-dev libharfbuzz-dev libfribidi-dev libxml2-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
RUN apt install -y librdf0-dev

# download and install R
RUN wget https://cloud.r-project.org/src/base/R-4/R-4.1.2.tar.gz
RUN tar -xzvf R-4.1.2.tar.gz
WORKDIR R-4.1.2
RUN ./configure
RUN make
RUN make install
WORKDIR /home/ubuntu

# install mlflow
RUN apt install -y sqlite3
RUN pip install mlflow
RUN pip install pyarrow
# install python libraries
ADD requirements.txt requirements.txt
RUN pip install -r requirements.txt
# install R libraries
RUN wget https://cran.r-project.org/contrib/main/00Archive/Matrix/Matrix_1.5-1.tar.gz
RUN printf "install.packages('Matrix_1.5-1.tar.gz', repos = NULL, type='source')" | Rscript -
RUN printf "for (pack in c('arules', 'glmnet', 'ranger', 'parsedate')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -
RUN printf "for (pack in c('zoo', 'roll', 'padr', 'quantreg', 'testthat')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -
RUN printf "for (pack in c('kernlab', 'fastDummies', 'caret', 'penalized')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -
RUN printf "for (pack in c('GA','coro', 'magrittr', 'rdflib', 'reticulate')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -
RUN printf "for (pack in c('gridExtra', 'plotly', 'carrier', 'mlflow', 'pastecs')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -
RUN printf "for (pack in c('matrixStats', 'qgam', 'devtools', 'pbs', 'ggplot2')){install.packages(pack, repos='http://cran.es.r-project.org')}"| Rscript -

RUN wget https://cran.r-project.org/src/contrib/Archive/onlineforecast/onlineforecast_1.0.0.tar.gz
RUN printf "install.packages('onlineforecast_1.0.0.tar.gz', repos = NULL, type='source')" | Rscript -

ADD biggr biggr
RUN R CMD INSTALL --preclean --no-multiarch --with-keep.source biggr

# install hdfs
RUN pip install pyhdfs
USER ubuntu