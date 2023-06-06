import rdflib
import argparse
import pandas as pd
from rdflib.namespace import RDFS

label_uri_map={
    "rdfs:label": "http://www.w3.org/2000/01/rdf-schema#label",
    "rdfs:comment": "http://www.w3.org/2000/01/rdf-schema#comment",
    "geo:officialName": "http://www.geonames.org/ontology#officialName",
    "dcterms:description": "http://purl.org/dc/terms/description"
}

uri_label_map = {v: k for k, v in label_uri_map.items()}

arg = argparse.ArgumentParser()
arg.add_argument("--source", "-s", required=True, help="the parttl file as source")
arg.add_argument("--headers", "-he", required=True, help="the headttl file")
arg.add_argument("--file", "-f", required=True, help="the excel file with translations")
arg.add_argument("--relationship", "-r", required=True, choices=label_uri_map.keys())
arg.add_argument("--create", "-c", action='store_true')
a = arg.parse_args()

"""
class A:
    pass

a = A()
a.headers = "/Users/eloigabal/Developement/CIMNE/bigg_entrack/ontology/dictionaries/bigg_enums/Headers.headttl"
a.source = "/Users/eloigabal/Developement/CIMNE/bigg_entrack/ontology/dictionaries/bigg_enums/UtilityType.parttl"
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
            graph.add((rdflib.URIRef(uri), rdflib.URIRef(label_uri_map[i.split("@")[0]]), rdflib.Literal(v, lang=str(i.split("@")[1]))))
    news = graph.serialize()

    for head in h.split("\n"):
        news = news.replace(head, '')

    with open(f"{a.source}_1", "w") as source:
        source.write(news)

