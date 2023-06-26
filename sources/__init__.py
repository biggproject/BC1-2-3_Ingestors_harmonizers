import json
import os
import pkgutil
from utils import utils
from datetime import datetime


class SourcePlugin(object):
    source_name = None

    def __init__(self, settings=None):
        self.settings = settings
        self.config = utils.read_config(settings.conf_file)
        self.config['source'] = self.source_name

    def gather(self, arguments):
        raise NotImplementedError

    def harmonizer_command_line(self, arguments):
        raise NotImplementedError

    def get_mapper(self, message):
        raise NotImplementedError

    def get_kwargs(self, message):
        raise NotImplementedError

    def get_store_table(self, message):
        raise NotImplementedError

    def transform_df(self, df):
        return df


class ManagedFile(object):
    NOT_PROCESSED = 'not_processed'
    PROCESSING = 'processing'
    PROCESSED = 'processed'
    FAILED = 'failed'

    @staticmethod
    def get_not_processed_path(path, auto):
        if auto:
            return f"{ManagedFile.normalize_path(path)}/{ManagedFile.NOT_PROCESSED}"
        else:
            return path

    @staticmethod
    def normalize_path(path):
        return path if not path.endswith('/') else path[:-1]

    def __init__(self, path, current_file):
        if os.path.isfile(f"{ManagedFile.normalize_path(path)}/{ManagedFile.NOT_PROCESSED}/{current_file}"):
            self.path = ManagedFile.normalize_path(path)
            self.current_file = current_file
            self.status = ManagedFile.NOT_PROCESSED
        else:
            raise FileNotFoundError(f"File {self.current_file} does not exists")

    def get_id(self, namespace, _id, auto):
        if not auto:
            return [namespace, _id]
        else:
            response = self.current_file.split("~")[1].split('#')
            response[0] = response[0].replace("@", '/') + '#'
            response[1] = response[1].split("-")[1]
            return response

    def get_file(self):
        return f'{self.path}/{self.status}/{self.current_file}'

    def set_status(self, status, auto):
        if not auto:
            return
        valid_changes = [(ManagedFile.NOT_PROCESSED, ManagedFile.PROCESSING),
                         (ManagedFile.PROCESSING, ManagedFile.PROCESSED), (ManagedFile.PROCESSING, ManagedFile.FAILED)]
        valid = False
        for old_s, new_s in valid_changes:
            if old_s == self.status and new_s == status:
                valid = True
        if not valid:
            raise Exception(f"File {self.current_file} is not in not_processed state")
        uid, node, created, processed, name = self.current_file.split("~")
        o_path = f"{self.path}/{self.status}/{self.current_file}"
        if self.status == ManagedFile.PROCESSING:
            processed = str(int(datetime.now().timestamp()))
        new_file = "~".join([uid, node, created, processed, name])
        n_path = f"{self.path}/{status}/{new_file}"
        os.makedirs("/".join(n_path.split("/")[:-1]), exist_ok=True)
        os.rename(o_path, n_path)
        self.current_file = new_file
        self.status = status
