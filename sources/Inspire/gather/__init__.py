import argparse
import json
import os
import pandas as pd
import utils
from utils.hbase import save_to_hbase
from utils.kafka import save_to_kafka
from utils.utils import log_string

def gather_building(args, config, settings):
    for file in os.listdir(args.file):
        if file.endswith('.json'):
            try:
                data_df = pd.read_json(f"{args.file}/{file}")
                data_df.dropna(how='all', axis='columns', inplace=True)
                data_df['data_type'] = 'building'
                save_inspire_data(data=data_df.to_dict(orient="records"), data_type="building",
                          row_keys=["Unique ID"],
                          column_map=[("info", "all")], config=config, settings=settings, args=args,
                          table_name=f"raw_{config['source']}_static_building__{args.user}")
            except Exception as ex:
                log_string(ex)

def gather_building_space(args, config, settings):
    for file in os.listdir(args.file):
        if file.endswith('.json'):
            try:
                data_df = pd.read_json(f"{args.file}/{file}")
                data_df['data_type'] = 'building_space'
                save_inspire_data(data=data_df.to_dict(orient="records"), data_type="building_space",
                          row_keys=["localId"],
                          column_map=[("info", "all")], config=config, settings=settings, args=args,
                          table_name=f"raw_{config['source']}_static_building_space__{args.user}")
            except Exception as ex:
                log_string(ex)

def gather_building_address(args, config, settings):
    for file in os.listdir(args.file):
        if file.endswith('.json'):
            try:
                data_df = pd.read_json(f"{args.file}/{file}")
                data_df['data_type'] = 'building_address'
                save_inspire_data(data=data_df.to_dict(orient="records"), data_type="building_address",
                          row_keys=["localId"],
                          column_map=[("info", "all")], config=config, settings=settings, args=args,
                          table_name=f"raw_{config['source']}_static_building_address__{args.user}")
            except Exception as ex:
                log_string(ex)

def save_inspire_data(data, data_type, row_keys, column_map, config, settings, args):
    if args.store == "kafka":
        try:
            k_topic = config["kafka"]["topic"]
            kafka_message = {
                "namespace": args.namespace,
                "user": args.user,
                "timezone": args.timezone,
                "collection_type": data_type,
                "source": config['source'],
                "row_keys": row_keys,
                "data": data
            }
            utils.kafka.save_to_kafka(topic=k_topic, info_document=kafka_message,
                                      config=config['kafka']['connection'], batch=settings.kafka_message_size)

        except Exception as e:
            utils.utils.log_string(f"error when sending message: {e}")

    elif args.store == "hbase":

        try:
            h_table_name = f"{config['data_sources'][config['source']]['hbase_table']}_ts_{data_type}_{args.user}"

            utils.hbase.save_to_hbase(data, h_table_name, config['hbase_store_raw_data'], column_map,
                                      row_fields=row_keys)
        except Exception as e:
            utils.utils.log_string(f"Error saving Inspire {data_type} to HBASE: {e}")
    else:
        utils.utils.log_string(f"store {config['store']} is not supported")


def gather(arguments, config=None, settings=None):
    ap = argparse.ArgumentParser(description='Gathering data from Inspire')
    ap.add_argument("-st", "--store", required=True, help="Where to store the data", choices=["kafka", "hbase"])
    ap.add_argument("--user", "-u", help="The user importing the data", required=True)
    ap.add_argument("--namespace", "-n", help="The subjects namespace uri", required=True)
    ap.add_argument("-f", "--file", required=True, help="Excel file path to parse")
    ap.add_argument("--timezone", "-tz", help="The local timezone", required=True, default='Europe/Madrid')
    ap.add_argument("-c", "--collection", required=True,
                    choices=['building', 'building_space', 'address'])
    args = ap.parse_args(arguments)


    if args.collection == 'building':
        gather_building(args, config, settings)

    if args.collection == 'building_space':
        gather_building_space(args, config, settings)

    if args.collection == 'address':
        gather_building_address(args, config, settings)


