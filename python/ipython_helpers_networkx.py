#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import networkx as nx
from networkx.classes.graph import Graph
from matplotlib import pyplot
import re
def subgraph_from_edges(g, edges, *args, **kwargs):
    res = type(g)(*args, **kwargs)
    res.graph = g.graph
    if not edges:
        return res
    if len(edges[0]) < 3:
        # not provided with data, get data from the original graph
        edges = [
            edge for edge in g.edges(data=True)
            if (edge[0], edge[1]) in edges
        ]
    def node_in_edges(node, edges):
        for edge in edges:
            if node[0] in edge[0:2]:
                return True
        return False
    # find nodes from the edges
    nodes = [node for node in g.nodes(data=True)
             if node_in_edges(node, edges)
    ]
    res.add_nodes_from(nodes)
    res.add_edges_from(edges)
    return res

def draw_networkx_with_edges(g, clf=True):
    if clf:
        pyplot.clf()
    pos = nx.spring_layout(g)
    nx.draw_networkx_edge_labels(g, pos)
    nx.draw_networkx(g, pos)

def all_edges(self, *args, **kwargs):
    return self.out_edges(*args, **kwargs) + self.in_edges(*args, **kwargs)

Graph.all_edges = all_edges
nx.subgraph_from_edges = subgraph_from_edges
nx.draw_networkx_with_edges = draw_networkx_with_edges


def to_agraph(N):
    """Return a pygraphviz graph from a NetworkX graph N.

    Parameters
    ----------
    N : NetworkX graph
      A graph created with NetworkX

    Examples
    --------
    >>> K5=nx.complete_graph(5)
    >>> A=nx.to_agraph(K5)

    Notes
    -----
    If N has an dict N.graph_attr an attempt will be made first
    to copy properties attached to the graph (see from_agraph)
    and then updated with the calling arguments if any.

    """
    try:
        import pygraphviz
    except ImportError:
        raise ImportError('requires pygraphviz ',
                          'http://networkx.lanl.gov/pygraphviz ',
                          '(not available for Python3)')
    directed=N.is_directed()
    strict=N.number_of_selfloops()==0 and not N.is_multigraph()
    A=pygraphviz.AGraph(name=N.name,strict=strict,directed=directed)

    # default graph attributes
    A.graph_attr.update(N.graph.get('graph',{}))
    A.node_attr.update(N.graph.get('node',{}))
    A.edge_attr.update(N.graph.get('edge',{}))

    # add nodes
    for n,nodedata in N.nodes(data=True):
        A.add_node(n,**nodedata)

    # loop over edges
    if N.is_multigraph():
        for u,v,key,edgedata in N.edges_iter(data=True,keys=True):
            str_edgedata=dict((k,v) for k,v in edgedata.items())
            A.add_edge(u,v,key=key,**str_edgedata)
    else:
        for u,v,edgedata in N.edges_iter(data=True):
            str_edgedata=dict((k,v) for k,v in edgedata.items())
            A.add_edge(u,v,**str_edgedata)


    return A
from networkx.drawing import nx_agraph
nx_agraph.to_agraph = to_agraph

def nx_find_node_by_name(g, *conjunctions):
    def find_node_by_name_one(g, conjunction):
        if isinstance(conjunction, basestring) :
            conjunction = [conjunction,]
        return [node for node in g.nodes()
                if not None in
                [re.search(name, node, flags=re.I) for name in conjunction]
        ]
    return __builtin__.sum([find_node_by_name_one(g, conjunction) for conjunction in conjunctions], [])

def nx_highligh_nodes(g, name_matches, highlight=None):
    if isinstance(name_matches, basestring) :
        name_matches = [name_matches,]
    highlight = highlight or {
        u"style" : u"filled",
        u"fillcolor" : u"blue",
        u"fontcolor" : u"white",
    }
    # reset the graph
    for node in g.nodes():
        for key in highlight.keys():
            if u"old-{}".format(key) in g.node[node]:
                old_value = g.node[node][u"old-{}".format(key)]
                #print(u"Restoring {} ({}) for {}".format(key, old_value, node))
                g.node[node][key] = old_value
                del g.node[node][u"old-{}".format(key)]
    def highligh_nodes_one(g, node):
        print(u"Highlighting {}".format(node))
        for key in highlight.keys():
            old_value = g.node[node].get(
                "old-{}".format(key),
                g.node[node].get(
                    key,
                    ""
                )
            )
            new_value = highlight[key]
            # print(u"Replacing {} by {} for {}".format(old_value,
            #                                           new_value,
            #                                           node))
            g.node[node][u"old-{}".format(key)] = old_value
            g.node[node][key] = new_value
    # find the nodes
    nodes = nx_find_node_by_name(g, *conjunctions)
    for node in nodes:
        highligh_nodes_one(g, node)


import uuid
import json
import networkx

def digraph_idraw(graph, width=2000, height=600):
    def create_node(graph, node):
        res = {"id":node}
        res.update(graph.node[node])
        return res
    nodes = [
        create_node(graph, node)
        for node in graph.nodes()
    ]
    edges = [
        {
            "from": edge[0],
            "to": edge[1],
            "arrows": "to",
        }
        for edge in graph.edges()
    ]
    uuid_ = uuid.uuid1()
    from IPython.display import HTML, display
    display(HTML(
    """
    <style type="text/css">
    #i{uuid} {{
    width: {width}px;
    height: {height}px;
    border: 1px solid lightgray;
    }}
    </style>
    <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.7.0/vis.css" rel="stylesheet" type="text/css">
    <script type='text/javascript'>
require(["https://cdnjs.cloudflare.com/ajax/libs/vis/4.7.0/vis.js"], function(lib) {{
    console.log("test");
    window.vis = jQuery.extend(true, {{}}, lib);
      var nodes = new vis.DataSet({nodes});

    // create an array with edges
    var edges = new vis.DataSet({edges});

    // create a network
    var container = document.getElementById('i{uuid}');

    // provide the data in the vis format
    var data = {{
        nodes: nodes,
        edges: edges
    }};
    var options = {{
/*    physics:  */
    }};

    // initialize your network!
    var network = new vis.Network(container, data, options);
    }});
    </script>
    <div id="i{uuid}">
    """.format(
        uuid=uuid_,
        nodes=json.dumps(nodes),
        edges=json.dumps(edges),
        width=width,
        height=height,
    )))

networkx.DiGraph.idraw = digraph_idraw
networkx.Graph.idraw = digraph_idraw
