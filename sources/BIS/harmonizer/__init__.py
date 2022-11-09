import argparse
import re
import utils
from utils.cache import Cache
from .mapper_static import harmonize_data


def harmonize_command_line(arguments, config=None, settings=None):
    ap = argparse.ArgumentParser(description='Mapping of GPG data to neo4j.')
    ap.add_argument("-o", "--organizations", action='store_true', help="Import the organization structure")
    ap.add_argument("--user", "-u", help="The user importing the data", required=True)
    ap.add_argument("--namespace", "-n", help="The subjects namespace uri", required=True)
    args = ap.parse_args(arguments)

    hbase_conn = config['hbase_store_raw_data']
    hbase_table = f"raw_BIS_static_buildings__{args.user}"
    i = 0
    Cache.load_cache()
    for data in utils.hbase.get_hbase_data_batch(hbase_conn, hbase_table, batch_size=1000):
        dic_list = []
        print("parsing hbase")
        for u_c, x in data:
            item = dict()
            for k, v in x.items():
                k1 = re.sub("^info:", "", k.decode())
                item[k1] = v
            item.update({"Unique Code": u_c})
            dic_list.append(item)
        print("parsed. Mapping...")
        i += len(dic_list)
        print(i)
        harmonize_data(dic_list, namespace=args.namespace, user=args.user,
                       organizations=args.organizations, config=config)
