from tempfile import NamedTemporaryFile
import sys
import rdflib
import time


def get_graph_single_query(subject):
    return f"""
    PREFIX bigg: <http://bigg-project.eu/ontology#>
    select ?p ?o
    where {{
    <{str(subject).strip()}> ?p ?o.
    }}
    """


def recursive_g(rdf_obj, subject_lst, graph, depth):
    if depth <= 0:
        return graph
    nxt_subjects = []
    for subject in subject_lst:
        q1 = get_graph_single_query(subject)
        r1 = rdf_obj.query(q1)
        for tri in r1:
            graph.add((subject, tri[0], tri[1]))
            if isinstance(tri[1], rdflib.URIRef):
                nxt_subjects.append(tri[1])
    return recursive_g(rdf_obj, nxt_subjects, graph, depth-1)


def get_subgraph_from_rdf(rdf_file, subject):
    rdf = rdflib.graph.Graph()
    rdf.parse(rdf_file)
    sub_graph = rdflib.graph.Graph()
    recursive_g(rdf, [rdflib.URIRef(subject)], sub_graph, 6)
    with NamedTemporaryFile(delete=False, suffix=".ttl", mode='w') as f:
        sub_graph.serialize(f.name, format="ttl")
    return f.name

