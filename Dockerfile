FROM docker.tech.beegroup-cimne.com/base_dockers/enma-job-3.10:latest as cached
USER root
RUN apt-get update
RUN python3 -m pip install --upgrade pip
USER ubuntu
WORKDIR bigg_entrack
COPY ontology ontology
COPY sources sources
COPY translations translations
COPY requirements.txt requirements.txt
COPY set_up_params.py set_up_params.py
COPY settings.py settings.py
RUN pip install -r requirements.txt


