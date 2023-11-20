import json
from tempfile import NamedTemporaryFile
import argparse
import neo4j
import pandas as pd
from pyhive import hive
import utils
import time
import os
import subprocess
import sys
import settings
import tarfile

m_name = "EEMLongitudinalBenchmarking"
input_mr = '/user/hive/bigg/EEMLongitudinalBenchmarking'

info_tables_list = [
    ("harmonized_online_CO2Emissions_100_SUM_PT1H_icaen", "harmonized_online_CO2Emissions_100_SUM_PT1H_icaen"),
    ("harmonized_online_EnergyConsumptionGas_000_SUM__icaen", "harmonized_online_EnergyConsumptionGas_000_SUM__icaen"),
    ("harmonized_online_EnergyConsumptionGridElectricity_100_SUM_PT1H_icaen", "harmonized_online_EnergyConsumptionGridElectricity_100_SUM_PT1H_icaen"),
    ("harmonized_online_Price_100_SUM_PT1H_icaen", "harmonized_online_Price_100_SUM_PT1H_icaen"),
    ("harmonized_online_Temperature_100_AVG_PT1H_public", "harmonized_online_Temperature_100_AVG_PT1H_public")
]

sensor_query = """
    SELECT ?hash (GROUP_CONCAT(distinct ?b; separator=",") as ?buildings) WHERE {
        ?m a bigg:Measurement .
        ?b bigg:hasSpace ?bs .
        ?bs bigg:isObservedByDevice ?d .
        ?d bigg:hasSensor ?s .
        ?s bigg:hasMeasurement ?m .
        BIND (STRAFTER(STR(?m),'#') as ?hash) .
    } GROUP BY ?hash
"""

tariff_query = """
    SELECT ?hash (GROUP_CONCAT(distinct ?b; separator=",") as ?buildings) WHERE {
        ?tcp a bigg:TariffComponentPoint .
        ?b bigg:hasSpace ?bs .
        ?bs bigg:hasUtilityPointOfDelivery ?upod .
        ?upod bigg:hasContractedTariff ?tc .
        ?tc bigg:hasTariff ?t .
        ?t bigg:hasTariffComponentList ?tcl .
        ?tcl bigg:hasTariffComponentPoint ?tcp .
        BIND (STRAFTER(STR(?tcp),'#') as ?hash) .
    } GROUP BY ?hash
"""

co2_query = """
    SELECT ?hash (GROUP_CONCAT(distinct ?b; separator=",") as ?buildings) WHERE {
        ?co2v a bigg:CO2EmissionsPoint .
        ?b bigg:hasSpace ?bs .
        ?bs bigg:hasUtilityPointOfDelivery ?upod .
        ?upod bigg:hasCO2EmissionsFactor ?co2f .
        ?co2f bigg:hasCO2EmissionsFactorList ?co2fl .
        ?co2fl bigg:hasCO2EmissionsFactorValue ?co2v .
        BIND (STRAFTER(STR(?co2v),'#') as ?hash) .
    } GROUP BY ?hash
"""

building_query = """
    SELECT distinct ?s WHERE {?s a bigg:Building}
"""


def get_hashes_from_rdf(graph, query):
    d = graph.query(query)
    return [{str(x[0]): x[1].split(",")} for x in list(d)]


def cypher_query(building_uri, skip, limit=1000):
    return f"""    
        CALL {{
            MATCH (n:bigg__Building)-[:bigg__hasSpace]->()-[:bigg__isAssociatedWithElement]->()-[:bigg__isAffectedByMeasure]-(m)
            WHERE n.uri=~"{building_uri}.*"
            RETURN DISTINCT n skip {skip} limit {limit}
        }} 
        WITH n 
        MATCH (d)<-[r*1..6]-(n)
        WHERE all(r1 in r WHERE TYPE(r1) =~"bigg__.*" and not TYPE(r1)="bigg__assessesSingleKPI" and not TYPE(r1) = "bigg__hasAnalyticalModel")
        RETURN n, [r1 in r WHERE NOT EXISTS(r1.selected) OR r1.selected=true] as r, d
    """


def create_hive_table_from_hbase(hbase_table, namespace):
    return f"""
        CREATE EXTERNAL TABLE IF NOT EXISTS {hbase_table[1]}(key struct<b:bigint, hash:string, ts_ini:bigint>, value float, isReal boolean, ts_end bigint) 
        ROW FORMAT DELIMITED COLLECTION ITEMS TERMINATED BY '~' 
        STORED BY 'org.apache.hadoop.hive.hbase.HBaseStorageHandler' 
        WITH SERDEPROPERTIES ('hbase.columns.mapping' = ':key, v:value, info:isReal, info:end') 
        TBLPROPERTIES ('hbase.table.name' = '{namespace}:{hbase_table[0]}')
    """


def create_input_file_from_tables(input_file_name, hbase_tables, devices):
    if devices:
        devices_list = ",".join([f"'{d}'" for d in devices])
        tables_queries = [f"(SELECT key.hash, key.ts_ini, ts_end, value, isReal FROM {t[1]} where key.hash IN ({devices_list}))" for t in hbase_tables]
    else:
        tables_queries = [f"(SELECT key.hash, key.ts_ini, ts_end, value, isReal FROM {t[1]})" for t in hbase_tables]
    union_part = " UNION ALL ".join(tables_queries)
    if input_file_name:
        return f"""
            INSERT OVERWRITE DIRECTORY '{input_file_name}' ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t' {union_part}
        """
    else:
        return union_part


def extract_test(building_uri, folder):
    os.makedirs(folder, exist_ok=True)
    config = utils.utils.read_config(settings.conf_file)
    query = cypher_query(building_uri, 0, limit=1)
    rdf = utils.neo4j_to_rdf.get_rdf_with_cyper_query(query, config['neo4j'])
    existing_hashes = {}
    b_hashes = get_hashes_from_rdf(rdf, sensor_query)
    b_hashes = [list(k.keys())[0] for k in b_hashes]
    existing_hashes["harmonized_online_EnergyConsumptionGas_000_SUM__icaen"] = b_hashes
    existing_hashes["harmonized_online_EnergyConsumptionGridElectricity_100_SUM_PT1H_icaen"] = b_hashes
    existing_hashes["harmonized_online_Temperature_100_AVG_PT1H_public"] = b_hashes
    b_hashes = get_hashes_from_rdf(rdf, tariff_query)
    b_hashes = [list(k.keys())[0] for k in b_hashes]
    existing_hashes["harmonized_online_Price_100_SUM_PT1H_icaen"] = b_hashes
    b_hashes = get_hashes_from_rdf(rdf, co2_query)
    b_hashes = [list(k.keys())[0] for k in b_hashes]
    existing_hashes["harmonized_online_CO2Emissions_100_SUM_PT1H_icaen"] = b_hashes
    hive_uri = config['hive']['host'] #"master1.internal"
    hive_db = config['hive']['db']
    hbase_namespace = config['hbase_store_harmonized_data']['table_prefix']
    rdf.serialize(f"{folder}/building.ttl", format="ttl")
    for table in existing_hashes.keys():
        cursor = hive.connect(hive_uri).cursor()
        cursor.execute(f"use {hive_db}", async_=False)
        print(table)
        query = create_hive_table_from_hbase((table, table), hbase_namespace)
        cursor.execute(query, async_=False)
        input_table_query = create_input_file_from_tables(None, [(table, table)], existing_hashes[table])
        cursor.execute(input_table_query, async_=False)
        df = pd.DataFrame.from_records(cursor.fetchall(), columns=["hash", "start", "end", "value", "isReal"])
        for hash, df in df.groupby("hash"):
            json_dict = df[["start", "end", "value", "isReal"]].to_dict(orient="records")
            json.dump({hash: json_dict}, open(f"{folder}/{hash}.json", "w"))


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description='Launching eem longitudinal benchmarking module')
    ap.add_argument("-n", "--namespace", required=True, help="The namespace for launching the data")
    ap.add_argument("--limit", "-l", type=int, default=1000)
    ap.add_argument("--partial", "-p", action="store_true")
    ap.add_argument("--extract", "-e", action="store_true")
    args = ap.parse_args()
    if args.extract:
        extract_test(args.namespace, "extract_data")
        exit(0)
    config = utils.utils.read_config(settings.conf_file)
    buildings_rdf = []
    skip = 0
    limit = args.limit
    hbase_namespace = config['hbase_store_harmonized_data']['table_prefix']
    hive_uri = config['hive']['host']
    hive_db = config['hive']['db']
    print("gathering graphs", end=": ")
    s = time.time()
    while True:
        query = cypher_query(args.namespace, skip, limit=limit)
        rdf = utils.neo4j_to_rdf.get_rdf_with_cyper_query(query, config['neo4j'])
        if rdf:
            buildings_rdf.append(rdf)
            skip += limit
        else:
            break
        if args.partial:
            break
    print(time.time()-s)

    print("writting graphs to files", end=": ")
    s = time.time()
    files_mr = []
    archives = []
    for i, graph in enumerate(buildings_rdf):
        with open(f"graph{i}.ttl", "w") as f: #NamedTemporaryFile(delete=False, suffix=".ttl", mode='w') as f:
            graph.serialize(f.name, format="turtle")
        files_mr.append(f"{f.name}#{f.name.split('/')[-1]}")
    print(time.time()-s)

    print("preparing_map_tar", end=": ")
    s = time.time()
    tar = tarfile.open("buildings_hash.tgz", 'w|gz')
    existing_hashes = []
    for graph in buildings_rdf:
        b_hashes = get_hashes_from_rdf(graph, sensor_query)
        existing_hashes.extend([list(k.keys())[0] for k in b_hashes])
        for b_hash in b_hashes:
            with open(f"{list(b_hash.keys())[0]}.json", "w") as f:
                json.dump(b_hash, f)
            tar.add(f.name)
        b_hashes = get_hashes_from_rdf(graph, tariff_query)
        existing_hashes.extend([list(k.keys())[0] for k in b_hashes])
        for b_hash in b_hashes:
            with open(f"{list(b_hash.keys())[0]}.json", "w") as f:
                json.dump(b_hash, f)
            tar.add(f.name)
        b_hashes = get_hashes_from_rdf(graph, co2_query)
        existing_hashes.extend([list(k.keys())[0] for k in b_hashes])
        for b_hash in b_hashes:
            with open(f"{list(b_hash.keys())[0]}.json", "w") as f:
                json.dump(b_hash, f)
            tar.add(f.name)
    tar.close()
    print(time.time()-s)
    # make the key-map of all buildings:
    print("preparing_map_keys", end=": ")
    building_key_map = {}
    buildings_subject_list = []
    for graph in buildings_rdf:
        buildings_subject_list.extend([str(x[0]) for x in graph.query(building_query)])
    for i, x in enumerate(buildings_subject_list):
        building_key_map[x] = i
    with open(f"building_key.keymap", "w") as f:  # NamedTemporaryFile(delete=False, suffix=".ttl", mode='w') as f:
        json.dump(building_key_map, f)
    files_mr.append(f"{f.name}#{f.name.split('/')[-1]}")
    print(time.time() - s)

    # print("preparing input file", end=": ")
    # s = time.time()
    # cursor = hive.connect(hive_uri).cursor()
    # cursor.execute(f"use {hive_db}", async_=False)
    # for table in info_tables_list:
    #     query = create_hive_table_from_hbase(table, hbase_namespace)
    #     cursor.execute(query, async_=False)
    # input_table_query = create_input_file_from_tables(input_mr, info_tables_list, existing_hashes)
    # cursor.execute(input_table_query, async_=False)
    # print(time.time()-s)

    print("launching mr job", end=": ")
    s = time.time()
    # Map Reduce
    MOUNTS = 'YARN_CONTAINER_RUNTIME_DOCKER_MOUNTS=/hadoop_stack:/hadoop_stack:ro'
    IMAGE = 'YARN_CONTAINER_RUNTIME_DOCKER_IMAGE=docker.tech.beegroup-cimne.com/mr/longitudinal_benchmarking_python:1'
    RUNTYPE = 'YARN_CONTAINER_RUNTIME_TYPE=docker'
    # create mapper and reducer tgz
    tar = tarfile.open("mapper_lib.tgz", 'w|gz')
    for x in os.listdir("mapper_lib"):
        tar.add(f"mapper_lib/{x}", arcname=x)
    tar.close()
    tar = tarfile.open("reducer_lib.tgz", 'w|gz')
    for x in os.listdir("reducer_lib"):
        tar.add(f"reducer_lib/{x}", arcname=x)
    tar.close()
    tar = tarfile.open("ontology.tgz", 'w|gz')
    for x in os.listdir("ontology"):
        tar.add(f"ontology/{x}", arcname=x)
    tar.close()
    archives.append("mapper_lib.tgz#mapper_lib")
    archives.append("reducer_lib.tgz#reducer_lib")
    archives.append("buildings_hash.tgz#buildings_hash")
    archives.append("ontology.tgz#ontology")
    files_mr.append("mapper.py#mapper.py")
    files_mr.append("reducer.R#reducer.R")
    files_mr.append(f"{settings.conf_file}#{settings.conf_file}")
    files_mr.append(f".env#.env")
    files_mr.append(f"settings.py#settings.py")

    subprocess.call(["hdfs", "dfs", "-rm", "-r", f'tmp/{m_name}'],
                    bufsize=4096, stdout=sys.stdout, stderr=sys.stderr)
    system_call = ['mapred', 'streaming',
                     f'-Dmapreduce.map.env="{MOUNTS},{IMAGE},{RUNTYPE}"',
                     f'-Dmapreduce.reduce.env="{MOUNTS},{IMAGE},{RUNTYPE}"',
                     f'-Dmapreduce.job.name={m_name}',
                     f'-Dmapred.reduce.tasks=10',
                     f'-Dmmapreduce.reduce.memory.mb=10240'
                     f'-Dmapred.map.tasks.speculative.execution=false',
                     f'-Dmapred.reduce.tasks.speculative.execution=false',
                     '-files', ",".join(files_mr),
                     "-archives", ",".join(archives),
                     '-mapper', 'mapper.py',
                     '-reducer', 'reducer.R',
                     '-input', input_mr,
                     '-output', f'tmp/{m_name}/output']
    print(" ".join(system_call), file=sys.stderr)
    subprocess.call(system_call,
                    bufsize=4096, stdout=sys.stdout, stderr=sys.stderr)
    print(time.time()-s)


