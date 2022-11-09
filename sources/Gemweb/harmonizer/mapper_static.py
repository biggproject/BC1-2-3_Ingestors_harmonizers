from functools import partial
from urllib.parse import urlparse
import pandas as pd
from neo4j import GraphDatabase
from rdflib import Namespace
import settings
from utils.cache import Cache
from utils.neo4j import get_all_buildings_id_from_datasource
from ontology.namespaces_definition import bigg_enums
from .Gemweb_mapping import Mapping
from utils.rdf.rdf_functions import generate_rdf
from utils.data_transformations import *
from .transform_functions import ref_cadastral

from utils.rdf.save_rdf import save_rdf_with_source, link_devices_with_source

bigg = settings.namespace_mappings['bigg']


def clean_prepare_all_df(df):
    df['device_subject'] = df.dev_gem_id.apply(partial(device_subject, source="GemwebSource"))
    utility_type_taxonomy = get_taxonomy_mapping(
        taxonomy_file="sources/Gemweb/harmonizer/EnergyTypeTaxonomy.xls",
        default="")
    df['hasDeviceType'] = df.tipus_submin.map(utility_type_taxonomy). \
        apply(lambda x: f"Meter.EnergyMeter.{x}" if x else f"Meter.EnergyMeter").\
        apply(partial(to_object_property, namespace=bigg_enums))


def clean_prepare_linked_df(df):
    df['building'] = df.num_ens.apply(building_subject)
    df['location_info'] = df.num_ens.apply(location_info_subject)
    country_dict = Cache.country_dic
    country_fuzz = partial(
        fuzzy_dictionary_match,
        map_dict=fuzz_params(country_dict, ['ns1:countryCode']),
        default=None
    )
    unique_country = df.pais.unique()
    country_map = {k: country_fuzz(k) for k in unique_country}
    df['hasAddressCountry'] = df.pais.map(country_map)
    province_dic = Cache.province_dic_ES
    prov_map = {}
    for label, value in country_map.items():
        df_group = df.groupby("pais").get_group(label)
        if value:
            province_fuzz = partial(fuzzy_dictionary_match,
                                    map_dict=fuzz_params(
                                        province_dic,
                                        ['ns1:name'],
                                        filter_query=f"SELECT ?s ?p ?o WHERE{{?s ?p ?o . ?s ns1:parentCountry <{value}>}}"
                                        ),
                                    default=None
                                    )
        else:
            province_fuzz = partial(fuzzy_dictionary_match,
                                    map_dict=fuzz_params(
                                        province_dic,
                                        ['ns1:name']
                                        ),
                                    default=None
                                    )
        unique_province = df_group.provincia.unique()
        prov_map = {k: province_fuzz(k) for k in unique_province}
        df.loc[df['pais']==label, 'hasAddressProvince'] = df_group.provincia.map(prov_map)
    municipality_dic = Cache.municipality_dic_ES
    for label, value in prov_map.items():
        df_group = df.groupby("provincia").get_group(label)
        if value:
            municipality_fuzz = partial(fuzzy_dictionary_match,
                                    map_dict=fuzz_params(
                                        municipality_dic,
                                        ['ns1:name'],
                                        filter_query=f"SELECT ?s ?p ?o WHERE{{?s ?p ?o . ?s ns1:parentADM2 <{value}>}}"
                                    ),
                                    default=None
                                    )
        else:
            municipality_fuzz = partial(fuzzy_dictionary_match,
                                    map_dict=fuzz_params(
                                        municipality_dic,
                                        ['ns1:name']
                                    ),
                                    default=None
                                    )
        unique_city = df_group.poblacio.unique()
        city_map = {k: municipality_fuzz(k) for k in unique_city}
        df.loc[df['provincia'] == label, 'hasAddressCity'] = df_group.poblacio.map(city_map)

        df['cadastral_info'] = df.observacionsbuilding.apply(ref_cadastral).apply(validate_ref_cadastral)

        df['building_space'] = df.num_ens.apply(building_space_subject)
        building_type_taxonomy = get_taxonomy_mapping(
                                         taxonomy_file="sources/Gemweb/harmonizer/BuildingUseTypeTaxonomy.xls",
                                         default="Other")
        df['hasBuildingSpaceUseType'] = df['subtipus'].map(building_type_taxonomy). \
            apply(partial(to_object_property, namespace=bigg_enums))

        df['gross_floor_area'] = df.num_ens.apply(partial(gross_area_subject, a_source="GemwebSource"))
        df['building_element'] = df.num_ens.apply(construction_element_subject)
        df['utility_point'] = df.cups.apply(delivery_subject)
        utility_type_taxonomy = get_taxonomy_mapping(
                                        taxonomy_file="sources/Gemweb/harmonizer/EnergyTypeTaxonomy.xls",
                                        default="")
        df['hasUtilityType'] = df.tipus_submin.map(utility_type_taxonomy).\
            apply(partial(to_object_property, namespace=bigg_enums))


def harmonize_data(data, **kwargs):
    namespace = kwargs['namespace']
    user = kwargs['user']
    config = kwargs['config']

    neo = GraphDatabase.driver(**config['neo4j'])
    n = Namespace(namespace)
    mapping = Mapping(config['source'], n)
    with neo.session() as ses:
        source_id = ses.run(
            f"""Match (o: {bigg}__Organization{{userID: "{user}"}})-[:hasSource]->(s:GemwebSource) 
                return id(s)""")
        source_id = source_id.single().get("id(s)")

    with neo.session() as ses:
        ids = get_all_buildings_id_from_datasource(ses, source_id, settings.namespace_mappings)
    # create num_ens column with parsed values in df
    df = pd.DataFrame.from_records(data)
    df = df.applymap(decode_hbase)
    df.loc[:, "source_id"] = source_id
    clean_prepare_all_df(df)
    df['num_ens'] = df['codi'].apply(id_zfill)
    df_linked = df[df['num_ens'].isin([str(i) for i in ids])]
    clean_prepare_linked_df(df_linked)
    df_unlinked = df[df['num_ens'].isin([str(i) for i in ids]) == False]

    # get all devices with linked buildings
    for linked, df_ in [("linked", df_linked), ('unlinked', df_unlinked)]:
        g = generate_rdf(mapping.get_mappings(linked), df_)
        save_rdf_with_source(g, config['source'], config['neo4j'])
    link_devices_with_source(df, n, config['neo4j'])

