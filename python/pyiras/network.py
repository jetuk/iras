# -*- coding: utf-8 -*-
"""
IRAS Network implemented in networkx graph

@author: James Tomlinson
"""


import networkx as nx


# Should this be a networkx subclass?
class IRASNetwork(object):
    """
    Representation of IRAS network


    """
    def __init__(self, graph=None):
        self.graph = graph


    @classmethod
    def from_inp_file(cls, filename):
        """
        Initialise graph
        """

        fh = open(filename, 'r')

        graph = nx.Graph()

        # Add nodes from INP file
        print 'Loading Nodes...'
        graph.add_nodes_from( IRASNode.from_inp_file(fh) )

        # Add links from INP file
        print 'Loading Links...'
        links = IRASLink.from_inp_file(fh)

        for link in links:

            link_nodes = []
            # Find from and to nodes
            for nid in (link.from_node_id, link.to_node_id):

                u = [n for n in graph.nodes_iter() if n.node_id == nid]
                if len(u) > 1:
                    raise ValueError('Multiple nodes with ID {} found for link {}.'.format(nid, link.name))
                elif len(u) == 0:
                    raise ValueError('No nodes with ID {} found for link {}.'.format(nid, link.name))

                link_nodes.append( u[0]  )

            graph.add_edge(*link_nodes, object=link)

        fh.close()

        return cls(graph=graph)

    def get_link(self, id):
        for u,v,data in self.graph.edges_iter(data=True):
            link = data['object']
            if link.link_id == id:
                return link

        raise ValueError('Link with ID {} not found.'.format(id))

    def get_node(self, id):
        for node in self.graph.nodes_iter():
            if node.node_id == id:
                return node

        raise ValueError('Node with ID {} not found.'.format(id))

    def reservoirs(self, ):
        return [n for n in self.graph.nodes_iter() if n.is_reservoir]


    def lakes(self, ):
        return [n for n in self.graph.nodes_iter() if n.is_lake]


    def gauged(self, ):
        return [n for n in self.graph.nodes_iter() if n.is_gauged]


    def demand_nodes(self, ):
        return [n for n in self.graph.nodes_iter() if n.is_demand]


    def draw_with_mpl(self, ax=None):

        if ax is None:
            import matplotlib.pyplot as plt
            fig, ax = plt.subplots()


        G = self.graph
        pos=nx.spring_layout(G)

        from collections import OrderedDict

        node_categories = OrderedDict()

        node_categories['reservoirs']={'method':self.reservoirs, 'color':'blue', 'shape':'v'}
        node_categories['lakes']={'method':self.lakes, 'color':'blue', 'shape':'o'}
        node_categories['gauged']={'method':self.gauged, 'color':'green', 'shape':'o'}
        node_categories['demand']={'method':self.demand_nodes, 'color':'red', 'shape':'o'}
        node_categories['other']={'method':self.graph.nodes, 'color':'yellow', 'shape':'o'}

        all_nodes = set()
        for ncategory, attrs in node_categories.iteritems():

            nodes_to_plot = set(attrs['method']()).difference(all_nodes)
            all_nodes = all_nodes.union(nodes_to_plot)


            nx.draw_networkx_nodes(G, pos, nodelist=nodes_to_plot,
                               node_shape=attrs['shape'], node_color=attrs['color'])



        nx.draw_networkx_edges(G, pos)
        nx.draw_networkx_labels(G,pos,fontsize=14, labels={n:n.name for n in G.nodes()})


class IRASNode(object):
    def __init__(self, node_id, name, elevation=0, storage_capacity=None,
                 initial_storage=0.0, is_gauged=False, rule_group_id=0,
                 is_reservoir=False, is_lake=False, is_aquifer=False,
                 is_demand=False, is_hydropower=False, is_pump=False):

        self.node_id = node_id
        self.name = name
        self.elevation = elevation
        self.storage_capacity = storage_capacity
        if self.storage_capacity:
            self.initial_storage = initial_storage

        self.rule_group_id = rule_group_id

        self.is_gauged = is_gauged
        self.is_reservoir = is_reservoir
        self.is_lake = is_lake
        self.is_aquifer = is_aquifer
        self.is_demand = is_demand
        self.is_hydropower = is_hydropower
        self.is_pump = is_pump

    def __repr__(self, ):
        return 'IRASNode: {}'.format(self.name)

    @classmethod
    def from_inp_file(cls, fh):
        """
        Returns a list of IRASNodes that are defined in the filehandle, fh.

        Format description:,

            The first part of the 'iras.inp' is the declaration of the nodes in the network.
            The declarations  must be sandwiched between the words 'NODES ' and 'END OF NODES' with no extra lines in between:

            NODES
            1  0 0          0          1 0 0 0 0 0 0 0 Gauge-A
            2  0 0          0          1 0 0 0 0 0 0 0 Gauge_B
            3  0 0          0          1 0 0 0 0 0 0 0 Jct-Gauge
            4  0 0          0          0 0 0 0 0 0 0 0 Cons_A
            5  0 100.000000 75.000000  1 0 0 1 0 1 0 0 Lake
            6  0 700.000000 350.000000 1 6 1 0 0 1 0 0 Res
            7  0 0          0          1 0 0 0 0 0 0 0 Gauge_C
            12 0 0          0          1 0 0 0 0 0 0 0 Agg_Tribs
            13 0 0          0          0 0 0 0 0 1 0 0 Env_Flw
            14 0 0          0          0 0 0 0 0 0 0 0 Cons_B
            END OF NODES

            Format for nodes lines (all columns must be included):
            (1)Node ID number       (does not have to be in numerical order)
            (2)Elevation            Elevation for non storage nodes that use pumping/power equations or bi-directional flow, otherwise 0
            (3)Storage Capacity     Only for LAKES and RESERVOIRS, otherwise 0
            (4)Initial Storage      Initial volumetric storage for S, RESERVOIRS, WETLANDS and AQUIFER Nodes, otherwise 0
            (5)Key GAUGE            1 if the node has a  time-series (gauge node), otherwise 0
            (6)Rule Group ID        The 'Node ID number' of the lead (independent) RESERVOIR in the RESERVOIR group.
            (7)Key RESERVOIR        1 if the node is a RESERVOIR, otherwise 0
            (8)Key LAKE             1 if the node is a LAKE, otherwise 0
            (9)Key AQUIFER          1 if the node is an AQUIFER or WETLAND, otherwise 0
            (10)Key DEMAND    	    1 if the node is a storage or flow DEMAND node, otherwise 0
            (11)Key Hydropower      If the node has an outgoing link with hydropower enabled on it, its Link ID is entered here, otherwise 0
            (12)Key Pump            If the node has an outgoing link with hydropower enabled on it, its Link ID is entered here, otherwise 0
            (13)Name                Node_name
        """

        row = fh.next()
        while not row.startswith('NODES'):
            row = fh.next()

        nodes = []
        row = fh.next() # First node
        while not row.startswith('END OF NODES'):
            values = row.split()
            if len(values) != 13:
                raise IOError('Node definition contains incorrect number of columns')

            nid = int(values[0])
            name = values[-1]

            kwds = {
                'elevation':float(values[1]),
                'storage_capacity':values[2]=='1',
                'initial_storage':float(values[3]),
                'is_gauged':values[4]=='1',
                'rule_group_id':int(values[5]),
                'is_reservoir':values[6]=='1',
                'is_lake':values[7]=='1',
                'is_aquifer':values[8]=='1',
                'is_demand':values[9]=='1',
                'is_hydropower':values[10]=='1',
                'is_pump':values[11]=='1',
            }


            nodes.append( cls(nid, name, **kwds) )

            row = fh.next()

        return nodes


class IRASLink(object):
    def __init__(self, link_id, name, from_node_id, to_node_id, length=0.0,
                 flow_capacity=None, initial_storage=None, is_diversion=False,
                 is_demand=False, bidirectional=False):

        self.link_id = link_id
        self.from_node_id = from_node_id
        self.to_node_id = to_node_id
        self.length = length
        self.flow_capacity = flow_capacity
        self.initial_storage = initial_storage
        self.is_diversion = is_diversion
        self.is_demand = is_demand
        self.bidirectional = bidirectional


    @classmethod
    def from_inp_file(cls, fh):
        """
        Returns a list of IRASLinks that are defined in the filehandle, fh.

        Format description:,

            LINKS
            1  1  3  0 0         0 0 0 0 Gauge-A_Jct-Gauge
            2  2  3  0 0         0 0 0 0 Gauge_B_Jct-Gauge
            3  3  4  0 35.000000 0 1 0 0 Jct-Gauge_Cons_A
            4  3  5  0 0         0 0 0 0 Jct-Gauge_Lake
            5  5  6  0 0         0 0 0 0 Lake_Res
            6  7  6  0 0         0 0 0 0 Gauge_C_Res
            7  4  5  0 0         0 0 0 0 Cons_A_Lake
            8  6  13 0 0         0 0 0 0 Res_Env_Flw
            9  12 13 0 0         0 0 0 0 Agg_Tribs_Env_Flw
            10 13 14 0 0         0 0 0 0 Env_Flw_Cons_B
            END OF LINKS

            !Format for links lines:
            (1)Link ID number     	(does not have to be in numerical order)
            (2)From NodeID number   'Upstream' Node ID
            (3)To NodeID  number	'Downstream' Node ID
            (4)Length 		        Length of Link (optional), otherwise 0
            (5)Flow Capacity    	Flow capacity of DIVERSION LINKS, DEMAND and DIVERSION/DEMAND links, otherwise 0
            (6)Initial Storage  	Initial Storage of UNIDIRECTIONAL, DIVERSION, DEMAND and DIVERSION/DEMAND links, otherwise 0
            (7)DIVERSION Key        1 if DIVERSION LINK or DIVERSION/DEMAND link
            (8)DEMAND Key           1 if  DEMAND or DIVERSION/DEMAND link
            (9)Two Way Key		    1 if bidirectional link (GROUNDWATER or BIDIRECTIONAL SURFACE link)
            (10)Name		        Link_name
        """

        row = fh.next()
        while not row.startswith('LINKS'):
            row = fh.next()

        links = []
        row = fh.next() # First node
        while not row.startswith('END OF LINKS'):
            values = row.split()
            if len(values) < 10:
                raise IOError('Link definition contains incorrect number of columns')

            lid = int(values[0])
            name = values[-1]

            kwds = {
                'from_node_id':int(values[1]),
                'to_node_id':int(values[2]),
                'length':float(values[3]),
                'flow_capacity':float(values[4]),
                'initial_storage':float(values[5]),
                'is_diversion':values[6]=='1',
                'is_demand':values[7]=='1',
                'bidirectional':values[8]=='1',
            }


            links.append( cls(lid, name, **kwds) )

            row = fh.next()

        return links


if __name__ == '__main__':

    data_dir = '../../london'

    from os.path import join

    N = IRASNetwork.from_inp_file(join(data_dir, 'iras.inp'))


    N.draw_with_mpl()
    import matplotlib.pyplot as plt

    plt.show()
