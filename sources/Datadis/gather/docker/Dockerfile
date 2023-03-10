FROM docker.tech.beegroup-cimne.com/base_dockers/mr-docker:3.10
RUN apt update
ADD requirements.txt requirements.txt
RUN pip install -r requirements.txt
USER ubuntu
CMD ["python3"]
