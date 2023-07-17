import numpy as np
import rdflib
import argparse
import pandas as pd
from rdflib.namespace import RDFS

label_uri_map={
    "bigg:formula": "http://bigg-project.eu/ontology#formula",
    "rdfs:label": "http://www.w3.org/2000/01/rdf-schema#label",
    "rdfs:comment": "http://www.w3.org/2000/01/rdf-schema#comment",
    "bigg:shortName": "http://bigg-project.eu/ontology#shortName",
    "geo:officialName": "http://www.geonames.org/ontology#officialName",
    "dcterms:description": "http://purl.org/dc/terms/description"
}
uri_label_map = {v: k for k, v in label_uri_map.items()}

arg = argparse.ArgumentParser()
arg.add_argument("--source", "-s", required=True, help="the parttl file as source")
arg.add_argument("--headers", "-he", required=True, help="the headttl file")
arg.add_argument("--file", "-f", required=True, help="the excel file with translations")
arg.add_argument("--create", "-c", action='store_true')
a = arg.parse_args()

"""
class A:
    pass

a = A()
a.create = False
a.headers = "/Users/eloigabal/Developement/CIMNE/bigg_entrack/ontology/dictionaries/bigg_enums/Headers.headttl"
a.source = "/Users/eloigabal/Developement/CIMNE/bigg_entrack/ontology/dictionaries/bigg_enums/UtilityType.parttl"
a.file = "/Users/eloigabal/Developement/CIMNE/bigg_entrack/ontology/enhancement/UtilityType.xlsx"
"""

with open(a.headers) as headf:
    h = headf.read()

with open(a.source) as source:
    s = source.read()

graph = rdflib.graph.Graph()
graph.parse(data=h+s, format='ttl')

if a.create:
    sub = graph.subjects(unique=True)
    sub = list(sub)
    df = pd.DataFrame.from_records(data={"uri": sub})
    df.sort_values("uri", inplace=True)
    df['pred_obj'] = df.uri.apply(lambda x: [y for y in list(graph.predicate_objects(x)) if str(y[0]) in label_uri_map.values()])
    df['pred_obj'] = df['pred_obj'].apply(lambda x: [(f"{uri_label_map[str(y[0])]}@{y[1].language if y[1].language else 'en'}", y[1]) for y in x])
    df = df.explode('pred_obj')
    df['pred'] = df['pred_obj'].apply(lambda x: x[0] if not pd.isna(x) else x)
    df['obj'] = df['pred_obj'].apply(lambda x: x[1] if not pd.isna(x) else x)
    df = pd.pivot_table(df[['uri', 'pred', 'obj']], values='obj', index='uri', columns='pred', aggfunc=lambda x: x)
    if df.empty:
        df = pd.DataFrame.from_records(data={"uri": list(sub)})
    else:
        df.reset_index(inplace=True)
    df.to_excel(a.file, index=False, header=True)
else:
    df = pd.read_excel(a.file)
    df.set_index("uri", inplace=True)
    for uri, lang_dic in df.iterrows():
        for i, v in lang_dic.items():
            try:
                predicate, lang = i.split("@")
            except:
                predicate = i
                lang = None
            if isinstance(v, str):
                to_remove = [x for x in graph.triples((rdflib.URIRef(uri), rdflib.URIRef(label_uri_map[predicate]), None))]
                if lang:
                    to_remove = [x for x in to_remove if x[2].language == lang]
                for r in to_remove:
                    graph.remove(r)
                if lang:
                    graph.add((rdflib.URIRef(uri), rdflib.URIRef(label_uri_map[predicate]), rdflib.Literal(v.strip(), lang=str(lang))))
                else:
                    graph.add((rdflib.URIRef(uri), rdflib.URIRef(label_uri_map[predicate]), rdflib.Literal(v.strip())))

    news = graph.serialize()

    for head in h.split("\n"):
        news = news.replace(head, '')

    news = news.replace(',\n        ', ', ')  # replace this to get a nicer ttl
    with open(f"{a.source}", "w") as source:
        source.write(news)

"""
class=("AreaType" "BuildingConstructionElementType" "BuildingSpaceUseType" "BuildingSystemElementType" "DeviceType" "EnergyEfficiencyMeasureType" "KeyPerformanceIndicator" "MeasuredProperty" "MeasuredPropertyComponent" "ModelType" "UtilityType")
for x in ${class[@]}; do
    python3 ontology/dictionaries/bigg_enums/translate.py -s ontology/dictionaries/bigg_enums/$x.parttl --headers ontology/dictionaries/bigg_enums/Headers.headttl --file ontology/enhancement/$x.xlsx -c;
    done

class=("AreaType" "BuildingConstructionElementType" "BuildingSpaceUseType" "BuildingSystemElementType" "DeviceType" "EnergyEfficiencyMeasureType" "KeyPerformanceIndicator" "MeasuredProperty" "MeasuredPropertyComponent" "ModelType" "UtilityType")

for x in ${class[@]}; do
    echo $x
    python3 ontology/dictionaries/bigg_enums/translate.py -s ontology/dictionaries/bigg_enums/$x.parttl --headers ontology/dictionaries/bigg_enums/Headers.headttl --file ontology/enhancement/$x.xlsx;
    done
"""
