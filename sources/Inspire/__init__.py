from sources import SourcePlugin
from sources.Inspire.gather import gather
from sources.Inspire.harmonizer import harmonize_command_line

from sources.Inspire.harmonizer.Inspire_mapping import harmonize_buildings_static, harmonize_buildings_space_static, \
    harmonize_address_static


class Plugin(SourcePlugin):
    source_name = "inspire"

    def gather(self, arguments):
        gather(arguments, settings=self.settings, config=self.config)

    def harmonizer_command_line(self, arguments):
        harmonize_command_line(arguments, config=self.config, settings=self.settings)

    def get_mapper(self, message):
        if message["collection_type"] == 'building':
            return harmonize_buildings_static
        elif message["collection_type"] == 'building_space':
            return harmonize_buildings_space_static
        elif message["collection_type"] == 'address':
            return harmonize_address_static

    def get_kwargs(self, message):
        return {
            "namespace": message['namespace'],
            "user": message['user'],
            "config": self.config,
            "timezone": message['timezone']
        }

    def get_store_table(self, message):
        if message['collection_type'] == "building":
            return f"raw_Inspire_ts_building__{message['user']}"
        elif message['collection_type'] == "building_space":
            return f"raw_Inspire_ts_building_space__{message['user']}"
        elif message['collection_type'] == "address":
            return f"raw_Inspire_ts_address__{message['user']}"
