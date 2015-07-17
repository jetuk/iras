import numpy as np


parameter_register = {}  # dict of parameters


class Parameter(object):
    """Base class for all parameters in the IRAS model.
    """
    __parameter__kwd__ = None

    class __metaclass__(type):
        """Metaclass to register Parameter subclasses.

        Only subclasses with __parameter__kwd__ not None are registered
        """
        def __init__(cls, name, bases, dict):
            type.__init__(cls, name, bases, dict)
            if cls.__parameter__kwd__ is not None:
                parameter_register[cls.__parameter__kwd__] = cls

    def __init__(self, policy_id):
        self.policy_id = policy_id

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''Classmethod to append the data to the IRASNetwork object and its
        nodes and links. Method should be implemented for subclasses.
        '''
        print 'load_from_inp_data not implemented for keyword "{}".'.format(cls.__parameter__kwd__)


class SysEvaporationParameter(Parameter):
    """Default evaporation rate must be set for surface storage nodes.
    """
    __parameter__kwd__ = 'SysEvaporation'

    def __init__(self, policy_id, evaporation_rate):
        Parameter.__init__(self, policy_id)
        self.evaporation_rate = evaporation_rate

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Target:
        (1)policy ID
        (2)default evaporation rate
        '''
        policy_id = int(values[0])
        evaporation_rate = float(values[1])

        param = cls(policy_id, evaporation_rate)
        return param


class NodeParameter(Parameter):
    """Base class for all node parameters
    """
    def __init__(self, node, policy_id, policy_group_id, ):
        Parameter.__init__(self, policy_id)
        self.node = node
        self.policy_group_id = policy_group_id


class LinkParameter(Parameter):
    """Base class for all node parameters
    """
    def __init__(self, link, policy_id, policy_group_id, ):
        Parameter.__init__(self, policy_id)
        self.link = link
        self.policy_group_id = policy_group_id


class EvaporationParameter(NodeParameter):
    """Evaporation table for RESERVOIR, LAKE and WETLAND nodes
    """
    __parameter__kwd__ = 'Evaporation'

    def __init__(self, node, policy_id, policy_group_id,
                 evaporation_rate):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.evaporation_rate = evaporation_rate


class RatingParameter(NodeParameter):
    """Rating tables for LAKE, WETLAND and RESERVOIR nodes

    Rating tables define surface elevation, surface area, seepage
    rate (flow rate), maximum release and minimum release as a
    function of storage volume
    """
    __parameter__kwd__ = 'Rating'

    def __init__(self, node, policy_id, policy_group_id,
                 elevation, area, volume, seepage,
                 min_release_rate, max_release_rate=None):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        # Convert data args to numpy arrays for convenient use later.
        self.elevation = np.array(elevation, ndmin=1)
        self.area = np.array(area, ndmin=1)
        self.volume = np.array(volume, ndmin=1)
        self.seepage = np.array(seepage, ndmin=1)
        self.min_release_rate = np.array(min_release_rate, ndmin=1)
        # max_release_rate is None unless data has been given.
        if max_release_rate is not None:
            self.max_release_rate = np.array(max_release_rate, ndmin=1)
        else:
            self.max_release_rate = np.array(np.inf, ndmin=1)

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Target:
        (1) Policy Group ID  (Yearly group of policies) this is only if annual policy changes occur (defined in iras.def and iras.pol), default is 1
        (2) Policy ID	     (Within year policy, i.e. season ID number), default is 1
        (3) Comp Type	     1 because it is a NODE (LINKS are 2)
        (4) ID	             Node ID number
        (5)  Surface Elevation
        (6)  Surface Area
        (7)  Volume
        (8)  Seepage Rate
        (9)  Maximum Release Rate for RESERVOIRS only, otherwise 0
        (10) LAKE Discharge or RESERVOIR Min Release
        '''
        policy_group_id, policy_id, comp_type, _id = map(int, values[:4])
        assert(comp_type == 1)
        (elevation, area, volume, seepage,
            max_release_rate, min_release_rate) = map(float, values[4:])
        node = network.get_node(_id)

        if hasattr(node, 'rating'):
            node.rating.append_table(elevation, area, volume, seepage,
                                          min_release_rate,
                                          max_release_rate=max_release_rate)
        else:
            param = cls(node, policy_id, policy_group_id,
                        elevation, area, volume, seepage,
                        min_release_rate, max_release_rate=max_release_rate)
            node.rating = param
            return param

    def append_table(self, elevation, area, volume, seepage,
                     min_release_rate, max_release_rate=None):
        self.elevation = np.r_[self.elevation,
                               np.array(elevation, ndmin=1)]
        self.area = np.r_[self.area, np.array(area, ndmin=1)]
        self.volume = np.r_[self.volume, np.array(volume, ndmin=1)]
        self.seepage = np.r_[self.seepage, np.array(seepage, ndmin=1)]
        self.min_release_rate = np.r_[self.min_release_rate,
                                      np.array(min_release_rate, ndmin=1)]
        _max_rate = np.inf if max_release_rate is None else max_release_rate
        self.max_release_rate = np.r_[self.max_release_rate,
                                      np.array(_max_rate, ndmin=1)]

class AllocationParameter(NodeParameter):
    """DIVERSION tables that define diversion functions to outgoing diversion
    links.
    """
    __parameter__kwd__ = 'Allocation'

    def __init__(self, node, policy_id, policy_group_id,
                 output, output_link, allocation):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.output_link = output_link
        # Convert data args to numpy arrays for convenient use later.
        self.output = np.array(output, ndmin=1)
        self.allocation = np.array(allocation, ndmin=1)

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Target:
        (1) Policy Group ID  (Yearly group of policies) this is only if annual policy changes occur (defined in iras.def and iras.pol), default is 1
        (2) Policy ID	     (Within year policy, i.e. season ID number), default is 1
        (3) Comp Type	     1 because it is a NODE (LINKS are 2)
        (4) ID	             Node ID number
        (5) Output	        Total water available to flow out of diversion node (DIVERSION nodes can be either surface flow or storage nodes but cannot be connected to any bidirectional links)
        (6) Out LINK ID     LINK ID of link to which allocation is going
        (7) Allocation	    Total Allocated to Link as function of value in (5)
        '''
        policy_group_id, policy_id, comp_type, _id = map(int, values[:4])
        assert(comp_type == 1)
        output = float(values[4])
        output_link_id = int(values[5])
        allocation = float(values[6])
        
        node = network.get_node(_id)
        if output_link_id == 0:
            output_link = None
        else:
            output_link = network.get_link(output_link_id)

        if hasattr(node, 'allocation'):
            try:
                param = node.allocation[output_link]
                param.append_table(output, allocation)
            except KeyError:
                param = cls(node, policy_id, policy_group_id, output,
                            output_link, allocation)
                node.allocation[output_link] = param
        else:
            param = cls(node, policy_id, policy_group_id, output,
                        output_link, allocation)
            node.allocation = {output_link: param}
            return param

    def append_table(self, output, allocation):
        self.output = np.r_[self.output, np.array(output, ndmin=1)]
        self.allocation = np.r_[self.allocation, np.array(allocation, ndmin=1)]



class TargetParameter(NodeParameter):
    """
    Targets are set for DEMAND nodes. They are either flow (non-storage DEMAND
    node) or storage (LAKES and RESERVOIR) targets.
    """
    __parameter__kwd__ = 'Target'

    def __init__(self, node, policy_id, policy_group_id,
                 target):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.target = target

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Target:
        (1) Policy Group ID  (Yearly group of policies) this is only if annual policy changes occur (defined in iras.def and iras.pol), default is 1
        (2) Policy ID	     (Within year policy, i.e. season ID number), default is 1
        (3) Comp Type	     1 because it is a NODE (LINKS are 2)
        (4) ID	             Node ID number
        (5) Target Flow or Storage
        (6) 0
        '''
        policy_group_id, policy_id, comp_type, _id = map(int, values[:4])
        assert(comp_type == 1)
        target = float(values[4])
        node = network.get_node(_id)

        param = cls(node, policy_id, policy_group_id, target)
        node.target = param
        return param


class SourceParameter(NodeParameter):
    """
    Setting a RESERVOIR as a Source for a DEMAND node makes the RESERVOIR
    release additional water to satisfy any deficit in that DEMAND node.
    DEMAND LINKS can also be set as Sources for DEMAND NODES. Water is
    allocated to DEMAND LINKS if their corresponding DEMAND node has a deficit.
    DEMAND LINKS that are directly downstream of a DEMAND node do not require
    to be labeled as Source in order to receive allocation
    """
    __parameter__kwd__ = 'Source'

    def __init__(self, node, policy_id, policy_group_id,
                 source_node_or_link, contribution_fraction):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.source_node_or_link = source_node_or_link
        self.contribution_fraction = contribution_fraction


class ReservoirGroup(NodeParameter):
    """

    """
    __parameter__kwd__ = 'Balance'

    def __init__(self, lead_node, policy_id, policy_group_id,
                 dependent_nodes=None, balances=None):
        NodeParameter.__init__(self, lead_node, policy_id, policy_group_id)
        self.dependent_nodes = dependent_nodes
        if dependent_nodes is None:
            self.dependent_nodes = []

        self.balances = balances
        if balances is None:
            self.balances = []


class ReservoirRuleParameter(NodeParameter):
    """
    RESERVOIR release rules can be defined for the Lead/Independent RESERVOIRS
    in a group. They define the supply based release from a RESERVOIR as a
    function of its volume and day of the year. Usually several policies for
    RESERVOIR rules are used.
    """
    __parameter__kwd__ = 'Rule'

    def __init__(self, node, policy_id, policy_group_id,
                 min_storage_begin, max_storage_begin,
                 min_release_begin, max_release_begin,
                 min_release_end, max_release_end,
                 min_storage_end, max_storage_end,
                 ):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.min_storage_begin = np.array(min_storage_begin, ndmin=1)
        self.max_storage_begin = np.array(max_storage_begin, ndmin=1)
        self.min_release_begin = np.array(min_release_begin, ndmin=1)
        self.max_release_begin = np.array(max_release_begin, ndmin=1)
        self.min_storage_end = np.array(min_storage_end, ndmin=1)
        self.max_storage_end = np.array(max_storage_end, ndmin=1)
        self.min_release_end = np.array(min_release_end, ndmin=1)
        self.max_release_end = np.array(max_release_end, ndmin=1)


class ReservoirBalance(object):
    """
    RESERVOIR balance tables control the supply based release from dependent
    reservoirs in a group. Their supply based release is a function of either
    the total volume of the group or of the volume in the lead/independent
    RESERVOIR.
    """
    def __init__(self, node, volume, lead_volume):
        self.node = node
        self.volume = np.array(volume, ndmin=1)
        self.lead_volume = np.array(lead_volume, ndmin=1)


class PowerParameter(LinkParameter):
    __parameter__kwd__ = 'Power'

    def __init__(self, link, policy_id, policy_group_id,
                 power_capacity, time_factor, turbine_elevation,
                 power_efficiency, min_flow):
        LinkParameter.__init__(self, link, policy_id, policy_group_id)

        self.power_capacity = power_capacity
        self.power_efficiency = power_efficiency
        self.time_factor = time_factor
        self.turbine_elevation = turbine_elevation
        self.min_flow = min_flow


class PumpParameter(LinkParameter):
    __parameter__kwd__ = 'Pump'

    def __init__(self, link, policy_id, policy_group_id,
                 pump_rate):
        LinkParameter.__init__(self, link, policy_id, policy_group_id)

        self.pump_rate = pump_rate


class PerformanceParameter(NodeParameter):
    __parameter__kwd__ = 'Performance'

    def __init__(self, node, policy_id, policy_group_id,
                 failure_threshold):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.failure_threshold = np.array(failure_threshold, ndmin=1)

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Target:
        (1) Policy Group ID  (Yearly group of policies) this is only if annual policy changes occur (defined in iras.def and iras.pol), default is 1
        (2) Policy ID	     (Within year policy, i.e. season ID number), default is 1
        (3) RESERVOIR ID
        (4) Failure Threshold (fraction of RESERVOIR capacity)
        '''
        policy_group_id, policy_id, _id = map(int, values[:3])
        failure_threshold = float(values[3])
        node = network.get_node(_id)

        if hasattr(node, 'performance'):
            node.performance.append_table(failure_threshold)
        else:
            param = cls(node, policy_id, policy_group_id, failure_threshold)
            node.performance = param
            return param

    def append_table(self, failure_threshold):
        self.failure_threshold = np.r_[self.failure_threshold,
                                       np.array(failure_threshold, ndmin=1)]


class CostParameter(LinkParameter):
    __parameter__kwd__ = 'Cost'

    def __init__(self, link, policy_id, policy_group_id,
                 energy_price, energy_per_unit_flow, energy_price_inflation):
        LinkParameter.__init__(self, link, policy_id, policy_group_id)
        self.energy_price = energy_price
        self.energy_per_unit_flow = energy_per_unit_flow
        self.energy_price_inflation = energy_price_inflation

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Values:
        (1)-(2) same as for 'Evaporation:'
        (3) Link ID
        (4) Energy Price
        (5) Energy Requirements per unit of flow through link (can be left 0 if Pumping calculations are performed instead)
        (6) Annual energy cost increase (Fractional)
        '''
        policy_group_id, policy_id, link_id = map(int, values[:3])
        energy_price, energy_per_unit_flow, energy_price_inflation = map(float, values[3:])
        link = network.get_link(link_id)

        param = cls(link, policy_id, policy_group_id, energy_price,
                    energy_per_unit_flow, energy_price_inflation)
        link.cost = param
        return param


class DemandReductionParameter(NodeParameter):
    """
    Demand for flow DEMAND nodes can be reduced when the volume in an
    affiliated reservoir goes below certain thresholds.
    """
    __parameter__kwd__ = 'DemRed'

    def __init__(self, node, policy_id, policy_group_id,
                 reservoir_node, failure_threshold, demand_reduction):
        NodeParameter.__init__(self, node, policy_id, policy_group_id)
        self.reservoir_node = reservoir_node
        self.failure_threshold = np.array(failure_threshold, ndmin=1)
        self.demand_reduction = np.array(demand_reduction, ndmin=1)

    @classmethod
    def load_from_inp_data(cls, values, network):
        '''
        Values:
        (1)-(2) same as for 'Evaporation:'
        (3) DEMAND Node ID
        (4) RESERVOIR Node ID
        (5) Failure threshold (Fraction of reservoir capacity)
        (6) Fractional reduction in Demand

        '''
        policy_group_id, policy_id, demand_id, reservoir_id = map(int, values[:4])
        failure_threshold = float(values[4])
        demand_reduction = float(values[5])
        demand_node = network.get_node(demand_id)
        reservoir_node = network.get_node(reservoir_id)

        if hasattr(demand_node, 'demand_reduction'):
            try:
                param = demand_node.demand_reduction[reservoir_node]
                param.append_table(failure_threshold, demand_reduction)
            except KeyError:
                param = cls(demand_node, policy_id, policy_group_id, reservoir_node,
                            failure_threshold, demand_reduction)
                demand_node.demand_reduction[reservoir_node] = param
        else:
            param = cls(demand_node, policy_id, policy_group_id, reservoir_node,
                        failure_threshold, demand_reduction)
            demand_node.performance = {reservoir_node: param}
            return param

    def append_table(self, failure_threshold, demand_reduction):
        self.failure_threshold = np.r_[self.failure_threshold,
                                       np.array(failure_threshold, ndmin=1)]
        self.demand_reduction = np.r_[self.demand_reduction,
                                      np.array(demand_reduction, ndmin=1)]
