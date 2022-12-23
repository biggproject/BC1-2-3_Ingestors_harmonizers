import hashlib
from datetime import timedelta

import numpy as np
import pandas as pd
import rdflib
from neo4j import GraphDatabase
from rdflib import Namespace
from slugify import slugify
from thefuzz import process

import settings
from utils.cache import Cache
from sources.Bulgaria.constants import enum_energy_efficiency_measurement_type, enum_energy_saving_type, eem_headers
from sources.Bulgaria.harmonizer.Mapper import Mapper
from utils.data_transformations import *
from utils.hbase import save_to_hbase
from utils.neo4j import create_sensor
from ontology.namespaces_definition import bigg_enums, units
from utils.rdf.rdf_functions import generate_rdf
from utils.rdf.save_rdf import save_rdf_with_source, link_devices_with_source


def set_taxonomy(df):
    df['Type of building'] = df['Type of building'].str.strip()
    building_type_taxonomy = get_taxonomy_mapping(
        taxonomy_file="sources/Bulgaria/harmonizer/BuildingSpaceUseTypeTaxonomy.xlsx",
        default="Other")
    df['buildingSpaceUseType'] = df['Type of building'].map(building_type_taxonomy).apply(partial(to_object_property,
                                                                                              namespace=bigg_enums))


def set_municipality(df):
    municipality_dic = Cache.municipality_dic_BG
    municipality_fuzz = partial(fuzzy_dictionary_match,
                            map_dict=fuzz_params(
                                municipality_dic,
                                ['ns1:name']
                            ),
                            default=None
                            )
    unique_municipality = df['Municipality'].unique()
    municipality_map = {k: municipality_fuzz(k) for k in unique_municipality}
    df.loc[:, 'hasAddressCity'] = df['Municipality'].map(municipality_map)


def clean_dataframe_building_info(df_orig, source):
    df = df_orig.copy(deep=True)
    df['subject'] = df['filename'] + '~' + df['id'].astype(str)
    df['location_org_subject'] = df['Municipality'].apply(slugify).apply(building_department_subject)
    df['organization_subject'] = df['subject'].apply(building_department_subject)
    df['building_subject'] = df['subject'].apply(building_subject)
    df['building_name'] = df.apply(lambda x: f"{x.Municipality}:{x.name}~{x['Type of building']}", axis=1)
    df['building_id'] = df['filename'].str.slice(-5) + '~' + df['id'].astype(str)
    df['location_subject'] = df['subject'].apply(location_info_subject)
    df['epc_date'] = pd.to_datetime(df['EPC_Date_Date'])
    df['epc_date_before'] = df['epc_date'] - timedelta(days=365)
    df['epc_date'] = (df['epc_date'].astype('datetime64[s]')).dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    df['epc_date_before'] = (df['epc_date_before'].astype('datetime64[s]')).dt.strftime("%Y-%m-%dT%H:%M:%SZ")
    df['epc_before_subject'] = df['subject'].apply(lambda x: x + '~before').apply(epc_subject)
    df['epc_after_subject'] = df['subject'].apply(lambda x: x + '~after').apply(epc_subject)

    df['building_space_subject'] = df['subject'].apply(building_space_subject)
    df['gross_floor_area_subject'] = df['subject'].apply(partial(gross_area_subject, a_source=source))
    df['element_subject'] = df['subject'].apply(construction_element_subject)
    df['device_subject'] = df['subject'].apply(partial(device_subject, source=source))
    df['utility_subject'] = df['subject'].apply(delivery_subject)
    df['project_subject'] = df['subject'].apply(project_subject)
    return df


def clean_dataframe_eem(df_orig, source):
    eem_columns = [r".*Savings_Liquid fuels.*", r".*Savings_Hard fuels.*", r".*Savings_Gas.*", r".*Savings_Others.*",
                   r".*Savings_Heat energy.*", r".*Savings_Electricity.*", r".*Savings_Total.*",
                   r".*Savings_Emission reduction.*", r".*Savings_Finacial savings.*", r".*Investments.*", r".*Payback.*"]
    df = df_orig.copy(deep=True)
    df['subject'] = df['filename'] + '~' + df['id'].astype(str)
    df['element_subject'] = df['subject'].apply(construction_element_subject)

    df['epc_date'] = pd.to_datetime(df['EPC_Date_Date'])
    df['project_subject'] = df['subject'].apply(project_subject)

    # melt df to get 1 row for measure
    df_eem_melt = df.melt(id_vars=["subject"], value_vars=[x for x in df.columns if any([re.match(c, x) for c in eem_columns])])
    df_eem_melt["measure_type"] = df_eem_melt.variable.apply(lambda x: x.split("_")[0])
    eem_type_taxonomy = get_taxonomy_mapping(
        taxonomy_file="sources/Bulgaria/harmonizer/EnergyEfficiencyMeasureTypeTaxonomy.xlsx",
        default=None)
    df_eem_melt["measure_type"] = df_eem_melt.measure_type.map(eem_type_taxonomy)
    df_eem_melt.dropna(inplace=True)
    df_eem_melt["measure_type_id"] = df_eem_melt["measure_type"]
    df_eem_melt["measure_type"] = df_eem_melt["measure_type"].apply(partial(to_object_property, namespace=bigg_enums))

    # split variable information
    df_eem_melt["unit"] = df_eem_melt.variable.apply(lambda x: x.split("_")[-1])
    df_eem_melt["field"] = df_eem_melt.variable.apply(lambda x: "_".join(x.split("_")[1:-1]))
    # create the subject and filter measures
    df_eem_melt['eem_subject'] = df_eem_melt.apply(lambda x: f"{x['subject']}-{x['measure_type_id']}", axis=1).apply(
        eem_subject)
    df_eem_subjects = df_eem_melt.loc[(df_eem_melt.field == "Investments") & (df_eem_melt.value.astype("float") > 0)].eem_subject
    df_eem_melt = df_eem_melt.loc[np.isin(df_eem_melt.eem_subject, df_eem_subjects)]
    # set the variables as columns
    df_eem = df_eem_melt[["eem_subject", "field", "value"]].pivot(index='eem_subject', columns='field')
    df_eem.reset_index(inplace=True)
    # add extra fields
    df_eem["subject"] = df_eem['eem_subject'].map(df_eem_melt[['eem_subject', 'subject']].drop_duplicates(subset=["eem_subject"]).set_index("eem_subject").subject)
    df_eem['measure_type'] = df_eem['eem_subject'].map(df_eem_melt[['eem_subject', 'measure_type']].drop_duplicates(subset=["eem_subject"]).set_index("eem_subject")['measure_type'])

    df_eem["epc_date"] = df_eem['subject'].map(df[['subject', 'epc_date']].set_index("subject").epc_date)
    df_eem["element_subject"] = df_eem['subject'].map(df[['subject', 'element_subject']].set_index("subject").element_subject)
    df_eem['GFA, m2'] = df_eem['subject'].map(df[['subject', 'GFA, m2']].set_index("subject")['GFA, m2'])
    df_eem['project_subject'] = df_eem['subject'].map(df[['subject', 'project_subject']].set_index("subject")['project_subject'])
    df_eem.columns = [x[0] if x[0] != "value" else x[1] for x in df_eem.columns]
    return df_eem


def set_source_id(df, user, connection):
    neo = GraphDatabase.driver(**connection)
    with neo.session() as session:
        source_id = session.run(f"""Match(bigg__Organization{{userID:"{user}"}})-[:hasSource]->(s) return id(s) as id""").data()[0]['id']
    df['source_id'] = source_id


def harmonize_static(data, **kwargs):
    namespace = kwargs['namespace']
    user = kwargs['user']
    n = Namespace(namespace)
    config = kwargs['config']
    df = pd.DataFrame.from_records(data)
    df = df.applymap(decode_hbase)
    set_source_id(df, user, config['neo4j'])
    set_taxonomy(df)
    set_municipality(df)
    mapper = Mapper(config['source'], n)
    df_building = clean_dataframe_building_info(df, config['source'])
    g_building = generate_rdf(mapper.get_mappings("building_info"), df_building)
    save_rdf_with_source(g_building, config['source'], config['neo4j'])
    link_devices_with_source(df_building, n, config['neo4j'])
    df_measures = clean_dataframe_eem(df, config['source'])
    g_measures = generate_rdf(mapper.get_mappings("eem_savings"), df_measures)
    save_rdf_with_source(g_measures, config['source'], config['neo4j'])
    #harmonize_ts(df_building.to_dict(orient="records"), namespace=namespace, user=user, config=config)


def harmonize_ts(data, **kwargs):
    namespace = kwargs['namespace']
    user = kwargs['user']
    n = Namespace(namespace)
    config = kwargs['config']
    freq = 'P1Y'

    df = pd.DataFrame.from_records(data)

    neo4j_connection = config['neo4j']

    # measured_property_list = ['EnergyConsumptionOil', 'EnergyConsumptionCoal',
    #                           'EnergyConsumptionGas', 'EnergyConsumptionOthers',
    #                           'EnergyConsumptionDistrictHeating',
    #                           'EnergyConsumptionGridElectricity', 'EnergyConsumptionTotal']
    #
    # measured_property_df = ['annual_energy_consumption_before_liquid_fuels',
    #                         'annual_energy_consumption_before_hard_fuels',
    #                         'annual_energy_consumption_before_gas', 'annual_energy_consumption_before_others',
    #                         'annual_energy_consumption_before_heat_energy',
    #                         'annual_energy_consumption_before_electricity',
    #                         'annual_energy_consumption_before_total_consumption']

    measured_property_list = ['EnergyConsumptionGas', 'EnergyConsumptionGridElectricity']

    measured_property_df = ['annual_energy_consumption_before_gas', 'annual_energy_consumption_before_electricity']

    neo = GraphDatabase.driver(**neo4j_connection)
    hbase_conn2 = config['hbase_store_harmonized_data']

    with neo.session() as session:
        for i in range(len(measured_property_list)):
            print(measured_property_list[i])
            for index, row in df.iterrows():
                device_uri = str(n[row['device_subject']])

                sensor_id = sensor_subject(config['source'], row['subject'], measured_property_list[i], "RAW",
                                           freq)
                sensor_uri = str(n[sensor_id])
                measurement_id = hashlib.sha256(sensor_uri.encode("utf-8"))
                measurement_id = measurement_id.hexdigest()
                measurement_uri = str(n[measurement_id])

                create_sensor(session=session, device_uri=device_uri, sensor_uri=sensor_uri, unit_uri=units["KiloW-HR"],
                              property_uri=bigg_enums[measured_property_list[i]], estimation_method_uri=bigg_enums.Naive,
                              measurement_uri=measurement_uri, is_regular=True,
                              is_cumulative=False, is_on_change=False, freq=freq, agg_func="SUM", dt_ini=pd.Timestamp(row['epc_date_before']),
                              dt_end=pd.Timestamp(row['epc_date']), ns_mappings=settings.namespace_mappings)
            reduced_df = df[[measured_property_df[i]]]

            reduced_df['listKey'] = measurement_id
            reduced_df['isReal'] = False
            reduced_df['bucket'] = ((pd.to_datetime(df['epc_date_before']).values.astype(int) // 10 ** 9) // settings.ts_buckets) % settings.buckets
            reduced_df['start'] = (pd.to_datetime(df['epc_date_before']).values.astype(int)) // 10 ** 9
            reduced_df['end'] = (pd.to_datetime(df['epc_date']).values.astype(int)) // 10 ** 9

            reduced_df.rename(
                columns={measured_property_df[i]: "value"},
                inplace=True)

            device_table = f"harmonized_online_{measured_property_list[i]}_100_SUM_{freq}_{user}"

            save_to_hbase(reduced_df.to_dict(orient="records"), device_table, hbase_conn2,
                          [("info", ['end', 'isReal']), ("v", ['value'])],
                          row_fields=['bucket', 'listKey', 'start'])

            period_table = f"harmonized_batch_{measured_property_list[i]}_100_SUM_{freq}_{user}"

            save_to_hbase(reduced_df.to_dict(orient="records"), period_table, hbase_conn2,
                          [("info", ['end', 'isReal']), ("v", ['value'])],
                          row_fields=['bucket', 'start', 'listKey'])
        print("finished")
