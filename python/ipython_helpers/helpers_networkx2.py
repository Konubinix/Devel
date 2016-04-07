#!/usr/bin/env python
# -*- coding:utf-8 -*-

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
