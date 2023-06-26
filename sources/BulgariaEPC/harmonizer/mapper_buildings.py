import hashlib
import itertools
from datetime import timedelta

import numpy as np
import numpy_financial as npf
import pandas as pd
import rdflib
import utils.mongo
from neo4j import GraphDatabase
from rdflib import Namespace
from slugify import slugify
from thefuzz import process

import settings
from utils.cache import Cache
from sources.BulgariaEPC.harmonizer.Mapper import Mapper
from utils.data_transformations import *
from utils.hbase import save_to_hbase
from utils.neo4j import create_sensor, create_KPI
from ontology.namespaces_definition import bigg_enums, units
from utils.rdf.rdf_functions import generate_rdf
from utils.rdf.save_rdf import save_rdf_with_source, link_devices_with_source


def set_municipality(df):
    municipality_dic = Cache.municipality_dic_BG
    municipality_fuzz = partial(fuzzy_dictionary_match,
                            map_dict=fuzz_params(
                                municipality_dic,
                                ['ns1:name', 'ns1:shortName', 'ns1:alternateName']
                            ),
                            default=None
                            )
    unique_municipality = df['municipality'].unique()
    municipality_map = {k: municipality_fuzz(k) for k in unique_municipality}
    df.loc[:, 'hasAddressCity'] = df['municipality'].map(municipality_map)


def clean_dataframe_building_info(df_orig, source):
    timezone = "Europe/Sofia"
    df = df_orig.copy(deep=True)
    df['subject'] = df['building_id'] + '-' + df['epc_id'].str.strip().str.replace(" ", "")
    df['epc_before_subject'] = df['subject'].apply(lambda x: x + '~before').apply(epc_subject)
    df['epc_after_subject'] = df['subject'].apply(lambda x: x + '~after').apply(epc_subject)

    df['epc_date'] = df['epc_id'].str.split('/').apply(lambda x: pd.to_datetime(x[1].strip(), format="%d.%m.%Y").tz_localize(timezone).tz_convert("UTC"))
    df['epc_date_before'] = df['epc_date'] - pd.DateOffset(years=1)
    df['epc_date'] = df['epc_date'].dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    df['epc_date_before'] = df['epc_date_before'].dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    df["energy_class_before"] = df["energy_class_before"].str.strip()
    df["energy_class_after"] = df["energy_class_after"].str.strip()
    df['building_subject'] = df['building_id'].apply(building_subject)

    df['location_subject'] = df['building_id'].apply(location_info_subject)
    df['timezone'] = timezone
    df['building_space_subject'] = df['building_id'].apply(building_space_subject)
    df['gross_floor_area_subject'] = df['building_id'].apply(partial(gross_area_subject, a_source=source))
    df['element_subject'] = df['building_id'].apply(construction_element_subject)
    df['project_subject'] = df['building_id'].apply(project_subject)
    return df


def prepare_all(df, user, config):
    set_municipality(df)
    df_building = clean_dataframe_building_info(df, config['source'])
    return df_building

# def harmonize_all(data, **kwargs):
#     """
#     This function will harmonize all data in one execution. It is only used when reading from HBASE directly as it
#     takes too long for kafka executions
#     :param data: The json list with the data
#     :param kwargs: the set of parameters for the harmonizer (user, namespace and config)
#     :return:
#     """
#     harmonize_static(data, **kwargs)
#     for s in range(0, 2):
#         harmonize_kpi(data, split=s, **kwargs)
#     for s in range(0, 25):
#         harmonize_eem_kpi(data, split=s, **kwargs)
#     harmonize_ts(data, **kwargs)


def harmonize_static(data, **kwargs):
    """
    This function will harmonize only the building information.
    :param data: The json list with the data
    :param kwargs: the set of parameters for the harmonizer (user, namespace and config)
    :return:
    """
    namespace = kwargs['namespace']
    user = kwargs['user']
    n = Namespace(namespace)
    config = kwargs['config']
    df = pd.DataFrame.from_records(data)
    df = df.applymap(decode_hbase)
    df_building = prepare_all(df, user, config)
    mapper = Mapper(config['source'], n)
    g_building = generate_rdf(mapper.get_mappings("building_info"), df_building)
    save_rdf_with_source(g_building, config['source'], config['neo4j'])
    # g_measures = generate_rdf(mapper.get_mappings("eem_savings"), df_measures)
    # save_rdf_with_source(g_measures, config['source'], config['neo4j'])
    print("harmonized_static")

