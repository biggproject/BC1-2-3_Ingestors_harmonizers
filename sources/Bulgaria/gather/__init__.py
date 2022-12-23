import argparse
import hashlib
import os
import re

import openpyxl
import pandas as pd

import utils
from utils.nomenclature import RAW_MODE


def gather_data(config, settings, args):
    for file in os.listdir(args.file):
        if file.endswith('.xlsx'):
            path = f"{args.file}/{file}"
            df = pd.read_excel(path, header=list(range(4)), sheet_name="List_residential_pilots")
            df.columns = ["_".join([f for f in c if not re.match("Unnamed:.*", f)]) for c in df.columns]
            df['filename'] = hashlib.md5(file.encode()).hexdigest()
            df['id'] = df.index

            save_data(data=df.to_dict(orient='records'), data_type="BuildingInfo",
                      row_keys=["filename", "id"],
                      column_map=[("info", "all")], config=config, settings=settings, args=args)

def save_data(data, data_type, row_keys, column_map, config, settings, args):
    if args.store == "kafka":
        try:
            k_topic = config["kafka"]["topic"]
            kafka_message = {
                "namespace": args.namespace,
                "user": args.user,
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
            h_table_name = utils.nomenclature.raw_nomenclature("Bulgaria", RAW_MODE.STATIC, data_type=data_type,
                                                               frequency="", user=args.user)

            utils.hbase.save_to_hbase(data, h_table_name, config['hbase_store_raw_data'], column_map,
                                      row_fields=row_keys)
        except Exception as e:
            utils.utils.log_string(f"Error saving datadis supplies to HBASE: {e}")
    else:
        utils.utils.log_string(f"store {config['store']} is not supported")


def gather(arguments, settings, config):
    ap = argparse.ArgumentParser(description='Gathering data from Bulgaria')
    ap.add_argument("-st", "--store", required=True, help="Where to store the data", choices=["kafka", "hbase"])
    ap.add_argument("--user", "-u", help="The user importing the data", required=True)
    ap.add_argument("--namespace", "-n", help="The subjects namespace uri", required=True)
    ap.add_argument("-f", "--file", help="Excel file path to parse", required=True)
    args = ap.parse_args(arguments)

    gather_data(config=config, settings=settings, args=args)
