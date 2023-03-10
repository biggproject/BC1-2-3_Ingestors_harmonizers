import json
from tempfile import NamedTemporaryFile

import neo4j
from pyhive import hive
import utils
import time
import os
import subprocess
import sys
import settings
import tarfile

config = utils.utils.read_config(settings.conf_file)
m_name = "longitudinalBenchmarking"
info_tables_list = [
    ("harmonized_online_CO2Emissions_100_SUM_PT1H_icaen", "harmonized_online_CO2Emissions_100_SUM_PT1H_icaen"),
    ("harmonized_online_EnergyConsumptionGas_000_SUM__icaen", "harmonized_online_EnergyConsumptionGas_000_SUM__icaen"),
    ("harmonized_online_EnergyConsumptionGridElectricity_100_SUM_PT1H_icaen", "harmonized_online_EnergyConsumptionGridElectricity_100_SUM_PT1H_icaen"),
    ("harmonized_online_Price.EnergyPriceGas_100_SUM_PT1H_icaen", "harmonized_online_PriceEnergyPriceGas_100_SUM_PT1H_icaen"),
    ("harmonized_online_Price.EnergyPriceGridElectricity_100_SUM_PT1H_icaen","harmonized_online_PriceEnergyPriceGridElectricity_100_SUM_PT1H_icaen"),
    ("harmonized_online_Temperature_100_AVG_PT1H_public", "harmonized_online_Temperature_100_AVG_PT1H_public")
]


def get_measurement_hash_buildings(building_uri):
    return f"""
        MATCH (n:bigg__Measurement)<-[:bigg__hasMeasurement]-(s)<-[:bigg__hasSensor]-(d)
              <-[:bigg__isObservedByDevice]-(bs)<-[:bigg__hasSpace]-(b)
        WHERE b.uri=~"{building_uri}"
        RETURN split(n.uri,"#")[1] as hash, COLLECT(b.uri) as buildings"""


def get_tariff_hash_buildings(building_uri):
    return f"""
        Match(n)<-[:bigg__hasTariffComponentPoint]-(tcl)<-[:bigg__hasTariffComponentList]-(t)
            <-[:bigg__hasTariff]-(ct)<-[:bigg__hasContractedTariff]-(upd)
            <-[:bigg__hasUtilityPointOfDelivery]-(bs)<-[:bigg__hasSpace]-(b) 
        WHERE b.uri=~"{building_uri}" 
        RETURN split(n.uri,"#")[1] as hash, collect(b.uri) as buildings"""


def get_co2_hash_buildings(building_uri):
    return f"""
        Match (n:bigg__CO2EmissionsPoint)<-[:bigg__hasCO2EmissionsFactorValue]-(co2l)
            <-[:bigg__hasCO2EmissionsFactorList]-(co2F)<-[:bigg__hasCO2EmissionsFactor]-(upd)
            <-[:bigg__hasUtilityPointOfDelivery]-(bs)<-[:bigg__hasSpace]-(b) 
        WHERE b.uri=~"{building_uri}" 
        RETURN split(n.uri,"#")[1] as hash, collect(b.uri) as buildings"""


def cypher_query(building_uri, skip, limit=1000):
    return f"""    
        CALL {{
            MATCH (n:bigg__Building) 
            WHERE n.uri=~"{building_uri}"
            RETURN n skip {skip} limit {limit}
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


def create_input_file_from_tables(input_file_name, hbase_tables):
    tables_queries = [f"(SELECT key.hash, key.ts_ini, ts_end, value, isReal FROM {t[1]})" for t in hbase_tables]
    union_part = " UNION ALL ".join(tables_queries)
    return f"""
        INSERT OVERWRITE DIRECTORY '{input_file_name}' ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t' {union_part}
    """


buildings_rdf = []
skip = 0
limit = 1000
namespace = "https://icaen.cat#.*"
hbase_namespace = "development_harmonized_data2"
hive_uri = "master1.internal"
hive_db = "bigg"
s = time.time()

print("gathering graphs")
while True:
    query = cypher_query(namespace, skip, limit=limit)
    rdf = utils.neo4j_to_rdf.get_rdf_with_cyper_query(query, config['neo4j'])
    if rdf:
        buildings_rdf.append(rdf)
        skip += limit
    else:
        break
print(time.time()-s)

print("writting graphs to files")
s = time.time()
files_mr = []
archives = []
for graph in buildings_rdf:
    with NamedTemporaryFile(delete=False, suffix=".ttl", mode='w') as f:
        graph.serialize(f.name, format="turtle")
    files_mr.append(f"{f.name}#{f.name.split('/')[-1]}")

print(time.time()-s)
print("preparing_map_tar")
s = time.time()
tar = tarfile.open("buildings_hash.tgz", 'w|gz')
measurement_query = get_measurement_hash_buildings(namespace)
tariff_query = get_tariff_hash_buildings(namespace)
co2_query = get_co2_hash_buildings(namespace)
driver = neo4j.GraphDatabase.driver(**config['neo4j'])
with driver.session() as session:
    d = session.run(measurement_query)
    for res in d:
        mapping = {res.get("hash"): res.get("buildings")}
        with open(f"{res.get('hash')}.json", "w") as f:
            json.dump(mapping, f)
        tar.add(f.name)
    d = session.run(tariff_query)
    for res in d:
        mapping = {res.get("hash") : res.get("buildings")}
        with open(f"{res.get('hash')}.json", "w") as f:
            json.dump(mapping, f)
        tar.add(f.name)
    d = session.run(co2_query)
    for res in d:
        mapping = {res.get("hash") : res.get("buildings")}
        with open(f"{res.get('hash')}.json", "w") as f:
            json.dump(mapping, f)
        tar.add(f.name)
tar.close()
print(time.time()-s)


print("preparing input file")
s = time.time()
input_mr = '/user/hive/bigg/longitudinalBenchmarking'
cursor = hive.connect(hive_uri).cursor()
cursor.execute(f"use {hive_db}", async_=False)
for table in info_tables_list:
    query = create_hive_table_from_hbase(table, hbase_namespace)
    cursor.execute(query, async_=False)
input_table_query = create_input_file_from_tables(input_mr, info_tables_list)
cursor.execute(input_table_query, async_=False)
print(time.time()-s)

print("launching mr job")
s = time.time()
# Map Reduce
MOUNTS = 'YARN_CONTAINER_RUNTIME_DOCKER_MOUNTS=/hadoop_stack:/hadoop_stack:ro'
IMAGE = 'YARN_CONTAINER_RUNTIME_DOCKER_IMAGE=docker.tech.beegroup-cimne.com/mr/longitudinal_benchmarking_python:1'
RUNTYPE = 'YARN_CONTAINER_RUNTIME_TYPE=docker'

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
                 f'-Dmapred.reduce.tasks=16',
                 f'-Dmmapreduce.reduce.memory.mb=10240'
                 f'-Dmapred.map.tasks.speculative.execution=false',
                 f'-Dmapred.reduce.tasks.speculative.execution=false',
                 '-files', ",".join(files_mr),
                 "-archives", ",".join(archives),
                 '-mapper', 'mapper.py',
                 '-reducer', 'reducer.R',
                 '-input', input_mr,
                 '-output', f'tmp/{m_name}/output']

print(" ".join(system_call))
subprocess.call(system_call,
                bufsize=4096, stdout=sys.stdout, stderr=sys.stderr)

print(time.time()-s)


