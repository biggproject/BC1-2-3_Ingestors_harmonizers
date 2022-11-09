import json
import pkgutil
from utils import utils


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
