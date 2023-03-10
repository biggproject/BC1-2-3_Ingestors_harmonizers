FROM docker.tech.beegroup-cimne.com/base_dockers/enma-jobs as cached
USER root
RUN apt-get update
RUN python3 -m pip install --upgrade pip
USER ubuntu
WORKDIR bigg_entrack
COPY --chown=ubuntu requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY --chown=ubuntu ontology ontology
COPY --chown=ubuntu sources sources
COPY --chown=ubuntu translations translations
COPY --chown=ubuntu set_up_params.py set_up_params.py
COPY --chown=ubuntu settings.py settings.py


