"""
This file is responsible for visualizing the network
"""
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
import math

MIN_EDGE_WEIGHT = 10
MAX_NODE_SIZE = 300



"""#############################################################################################"""
"""
                                ACCESSING RAW CSV FILES
"""
"""#############################################################################################"""
# COMMUNITIES
def get_pure_comm_nodes():
    db_name = "../data/db/PURE_COMMS.csv"
    print("Reading rows from {}...".format(db_name))
    """
    """
    try:
        membs = pd.read_csv(db_name)
        return membs.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None
def get_sub_comm_nodes(comm_id):
    db_name = "../data/misc/COMM_{}_SUB_COMMS.csv".format(comm_id)
    """
    """
    try:
        membs = pd.read_csv(db_name)
        return membs.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None
# NODES
def get_pure_nodes():
    db_name = "../data/db/PURE_NODES.csv"
    print("Reading rows from {}...".format(db_name))
    """
    """
    try:
        nodes = pd.read_csv(db_name)
        return nodes.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None
# EDGES
def get_pure_edges():
    db_name = "../data/db/PURE_EDGES.csv"
    print("Reading rows from {}...".format(db_name))
    """
    """
    try:
        edges = pd.read_csv(db_name)
        return edges.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

"""#############################################################################################"""
"""
                                    FILTERING DATASETS
"""
"""#############################################################################################"""
# COMMUNITIES
def get_comm_nodes( nodes, comm_id ):          # Gets all the nodes within the given community
    comm_nodes = list([])

    # iterate through edges; check for comm_id match
    for node in nodes:
        if node[3] == comm_id:
            comm_nodes.append(node)

    # return comm_nodes
    return comm_nodes

def get_comm_edges( comm_id ):          # Gets all the nodes within the given community
    pass

# NODES
def get_nodes_from_edges( edges ):      # Gets all the nodes that were in the edges
    print("Getting nodes from {} edges.".format(len(edges)))
    # create dictionary of nodes
    nodes = get_pure_nodes()
    node_dict = dict()
    for node in nodes:
        key = str(node[0]).lower()
        node_dict[key] = node

    # iterate through edges; check for key match in nodes dictionary; append if match; print debug if no match
    edge_node_dict = dict()
    for edge in edges:
        key_a = str(edge[0]).lower()
        key_b = str(edge[1]).lower()
        if key_a in node_dict:
            node_a = node_dict[key_a]
            edge_node_dict[key_a] = node_a
        else: # not found, print warning
            print("< WARNING > : Key Not Found : ".format(key_b))
        if key_b in node_dict:
            node_b = node_dict[key_b]
            edge_node_dict[key_b] = node_b
        else: # not found, print warning
            print("< WARNING > : Key Not Found : ".format(key_b))

    # return edge_nodes
    return edge_node_dict.values()

def get_popular_nodes( nodes, node_count ):      # Gets "n" nodes that sent and received the most emails
    print(">>> Getting {} popular nodes from {} nodes.".format(node_count, len(nodes)))
    popular_nodes = nodes.copy()
    # sort nodes by popularity
    n = len(popular_nodes)
    # traverse through all array elements
    for i in range(n):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # traverse the array from 0 to n-i-1
            # Swap if the element found is lesser
            # than the next element
            more_popular = popular_nodes[j].copy()
            less_popular = popular_nodes[j+1].copy()

            more_popularity = more_popular[1] + more_popular[2]
            less_popularity = less_popular[1] + less_popular[2]

            if more_popularity < less_popularity :
                popular_nodes[j] = less_popular.copy()
                popular_nodes[j+1] = more_popular.copy()

    # return "n" popular nodes
    result = popular_nodes[:node_count]
    print("<<< Obtained {} popular nodes.".format(len(result)))
    return result

def get_sender_nodes( nodes, n ):       # Gets "n" nodes that sent the most emails
    print("Getting {} nodes that sent the most emails; from {} nodes.".format(n, len(nodes)))
    sender_nodes = nodes.copy()
    # sort nodes by send volume
    n = len(sender_nodes)
    # traverse through all array elements
    for i in range(n):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # traverse the array from 0 to n-i-1
            # Swap if the element found is greater
            # than the next element
            more_sender = sender_nodes[j]
            less_sender = sender_nodes[j+1]

            more_sent = more_sender[1]
            less_sent = less_sender[1]

            if more_sent < less_sent :
                sender_nodes[j], sender_nodes[j+1] = sender_nodes[j+1], sender_nodes[j]

    # return "n" sender nodes
    return sender_nodes[:n]

def get_receiver_nodes( nodes, n ):     # Gets "n" nodes that received the most emails
    print("Getting {} nodes that received the most emails; from {} nodes.".format(n, len(nodes)))
    receiver_nodes = nodes.copy()
    # sort nodes by send volume
    n = len(receiver_nodes)
    # traverse through all array elements
    for i in range(n):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # traverse the array from 0 to n-i-1
            # Swap if the element found is greater
            # than the next element
            more_receiver = receiver_nodes[j]
            less_receiver = receiver_nodes[j+1]

            more_received = more_receiver[2]
            less_received = less_receiver[2]

            if more_sent < less_sent :
                receiver_nodes[j], receiver_nodes[j+1] = receiver_nodes[j+1], receiver_nodes[j]

    # return "n" receiver nodes
    return receiver_nodes[:n]

# EDGES
def get_edges_from_nodes( nodes ):      # Gets all edges that occur between the given nodes
    print("Getting edges from {} nodes.".format(len(nodes)))
    # create dictionary of nodes
    node_dict = dict()
    for node in nodes:
        key = str(node[0]).lower()
        node_dict[key] = node

    # iterate through edges; check for key match in nodes dictionary; append if match; print debug if no match
    edges = get_pure_edges()
    node_edges = list([])
    for edge in edges:
        key_a = str(edge[0]).lower()
        key_b = str(edge[1]).lower()
        if key_a in node_dict and key_b in node_dict:
            node_edges.append(edge)

    # return node_edges
    return node_edges

def get_strongest_edges( edges, n ):    # Gets "n" edges that communicated the most
    print("Getting {} edges with the greatest weight; from {} edges.".format(n, len(edges)))
    sorted_edges = edges.copy()
    n = len(sorted_edges)

    # Traverse through all array elements
    for i in range(n):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # traverse the array from 0 to n-i-1
            # Swap if the element found is greater
            # than the next element
            strong_edge = sorted_edges[j]
            weak_edge = sorted_edges[j+1]

            if strong_edge[2] < weak_edge[2] :
                sorted_edges[j], sorted_edges[j+1] = sorted_edges[j+1], sorted_edges[j]

    # return "n" strongest edges
    return sorted_edges[:n]



"""#############################################################################################"""
"""
                                    NETWORK ATTRIBUTES
"""
"""#############################################################################################"""
# NODE
def get_node_behavior_colors(nodes, max_emails_sent, max_emails_rec):
    print(">>> Getting node colors from {} nodes.".format(len(nodes)))
    node_colors = list([])

    # Calculate node size then color; append to list
    for node in nodes:
        emails_sent = int(node[1])
        emails_rec = int(node[2])

        sent_ratio = (emails_sent / max_emails_sent) * 2
        rec_ratio = (emails_rec / max_emails_rec) * 2

        if sent_ratio > 0.5 or rec_ratio > 0.5:
            if sent_ratio > 0.5 and rec_ratio > 0.5:
                node_color = 'purple'
                node_colors.append(node_color)
                continue
            elif sent_ratio > 0.5:
                node_color = 'red'
                node_colors.append(node_color)
            else:
                node_color = 'blue'
                node_colors.append(node_color)
        else:
            red = int(sent_ratio * 255) % 255
            green = 0
            blue = int(rec_ratio * 255) % 255

            node_color = '#%02x%02x%02x' % (red, green, blue)
            node_colors.append(node_color)

    print(">>> Obtained {} node colors.".format(len(node_colors)))
    return node_colors

def get_node_community_colors(nodes, sub_comm=False):
    print(">>> Getting node colors from {} nodes.".format(len(nodes)))
    node_colors = list([])

    index = 3
    if sub_comm:
        index = 4

    # Determine node color from community; append to list
    for node in nodes:
        hex_color = "#ffffff"

        if node[index] == 0:
            hex_color = "#ff0000"   # red
        elif node[index] == 1:
            hex_color = "#00ff00"   # lime green
        elif node[index] == 2:
            hex_color = "#0000ff"   # blue
        elif node[index] == 3:
            hex_color = "#ffff00"   # yellow
        elif node[index] == 4:
            hex_color = "#660066"   # purple
        elif node[index] == 5:
            hex_color = "#00ffff"   # cyan
        elif node[index] == 6:
            hex_color = "#ffa500"   # orange
        elif node[index] == 7:
            hex_color = "#ff00ff"   # magenta
        elif node[index] == 8:
            hex_color = "#ffc0cb"   # pink
        elif node[index] == 9:
            hex_color = "#d3ffce"   # seafoam
        elif node[index] == 10:
            hex_color = "#000000"   # dark green
        elif node[index] == 11:
            hex_color = "#003366"   # navy
        elif node[index] == 12:
            hex_color = "#990000"   # brown
        elif node[index] == 13:
            hex_color = "#c0c0c0"   # grey
        elif node[index] == 14:
            hex_color = "#bada55"   # pale green
        else:
            hex_color = "#000000"

        node_colors.append(hex_color)

    print(">>> Obtained {} node colors.".format(len(node_colors)))
    return node_colors

def get_node_popularity_sizes(nodes, max_emails):
    print(">>> Getting node sizes from {} nodes.".format(len(nodes)))
    node_sizes = list([])

    # Calculate node sizes; append to list
    for node in nodes:
        emails = int(node[1]) + int(node[2])
        size = (emails / max_emails) * MAX_NODE_SIZE
        node_sizes.append(size)

    # return node sizes
    print("<<< Obtained {} node sizes.".format(len(node_sizes)))
    return node_sizes.copy()

def get_node_labels(nodes, label_rate, max_emails):
    print(">>> Getting node labels from {} nodes.".format(len(nodes)))
    node_labels = dict()

    # Calculate node sizes; append to list
    for node in nodes:
        emails = int(node[1]) + int(node[2])
        ratio = emails / max_emails
        if ratio > label_rate:
            label = node[0]
            node_labels[label] = label

    # return node sizes
    print("<<< Obtained {} node labels.".format(len(node_labels.values())))
    return node_labels

# EDGE


"""#############################################################################################"""
"""
                                    BUILDING NETWORKS
"""
"""#############################################################################################"""
def build_sr_network( node_count ):
    if node_count is None:
        print(">>> Building send-receive network with all nodes.")
    else:
        print(">>> Building send-receive network with {} nodes.".format(node_count))
    # make graph object
    G = nx.Graph()

    # obtain nodes for graph
    def_nodes = get_pure_nodes()
    nodes_list = list([])
    if node_count is None:
        nodes_list = def_nodes
    else:
        nodes_list = get_popular_nodes( def_nodes, node_count )
    # add nodes to graph
    G.add_nodes_from( [node[0] for node in nodes_list] )

    # obtain edges for graph
    edges_list = get_edges_from_nodes( nodes_list )
    # add edges to graph
    G.add_edges_from( [[edge[0], edge[1]] for edge in edges_list] )

    # obtain node attributes
    max_emails = max( [node[1]+node[2] for node in nodes_list] )
    max_emails_sent = max( [node[1] for node in nodes_list] )
    max_emails_rec = max( [node[2] for node in nodes_list] )
    node_colors = get_node_behavior_colors(nodes_list, max_emails_sent, max_emails_rec)
    node_sizes = get_node_popularity_sizes(nodes_list, max_emails)
    node_labels_dict = get_node_labels(nodes_list, 0.5, max_emails)
    print("<<< Built send-receive network with {} nodes.".format(len(G.nodes)))

    # debug
    print(nx.info(G))

    # draw network
    g_pos = nx.kamada_kawai_layout(G)
    nx.draw(G,pos=g_pos,node_color=node_colors,node_size=node_sizes,width=0.05)
    nx.draw_networkx_labels(G,g_pos,node_labels_dict,font_size=10)
    plt.show()

    return

def build_ct_network( comm_count ):
    if comm_count is None:
        print(">>> Building community network with all communities.")
    else:
        print(">>> Building communities network with {} communites.".format(comm_count))

    # make graph object
    G = nx.Graph()

    # obtain nodes for graph
    def_nodes = get_pure_comm_nodes()
    nodes_list = list([])
    if comm_count is None:
        nodes_list = def_nodes
    else:
        for comm_id in range(comm_count):
            if comm_id == 3 or comm_id == 4:
                continue
            nodes_list.extend(get_comm_nodes( def_nodes, comm_id ))
    # add nodes to graph
    G.add_nodes_from( [node[0] for node in nodes_list] )

    # obtain edges for graph
    edges_list = get_edges_from_nodes( nodes_list )
    # add edges to graph
    G.add_edges_from( [[edge[0], edge[1]] for edge in edges_list] )

    # obtain node attributes
    max_emails = max( [node[1]+node[2] for node in nodes_list] )
    max_emails_sent = max( [node[1] for node in nodes_list] )
    max_emails_rec = max( [node[2] for node in nodes_list] )
    node_colors = get_node_community_colors(nodes_list)
    node_sizes = get_node_popularity_sizes(nodes_list, max_emails)
    #node_labels_dict = get_node_labels(nodes_list, 0.5, max_emails)
    print("<<< Built community network with {} nodes.".format(len(G.nodes)))

    # debug
    print(nx.info(G))

    # centraility metrics
    centrality_metrics(G)

    # draw network
    g_pos = nx.kamada_kawai_layout(G)
    nx.draw(G,pos=g_pos,node_color=node_colors,node_size=node_sizes,width=0.05)
    #nx.draw_networkx_labels(G,g_pos,node_labels_dict,font_size=16)
    plt.show()

    return

def build_comm_network( comm_id, debug):
    print(">>> Building community network.")

    # make graph object
    G = nx.Graph()

    # obtain nodes for graph
    def_nodes = get_pure_comm_nodes()
    nodes_list = get_comm_nodes( def_nodes, comm_id )
    # add nodes to graph
    G.add_nodes_from( [node[0] for node in nodes_list] )

    # obtain edges for graph
    edges_list = get_edges_from_nodes( nodes_list )
    # add edges to graph
    G.add_edges_from( [[edge[0], edge[1]] for edge in edges_list] )

    # obtain node attributes
    max_emails = max( [node[1]+node[2] for node in nodes_list] )
    max_emails_sent = max( [node[1] for node in nodes_list] )
    max_emails_rec = max( [node[2] for node in nodes_list] )
    node_colors = get_node_community_colors(nodes_list)
    node_sizes = get_node_popularity_sizes(nodes_list, max_emails)
    node_labels_dict = get_node_labels(nodes_list, 0.95, max_emails)
    print("<<< Built community network with {} nodes.".format(len(G.nodes)))

    # debug
    print(nx.info(G))

    # draw network
    g_pos = nx.kamada_kawai_layout(G)
    nx.draw(G,pos=g_pos,node_color=node_colors,node_size=node_sizes,width=0.05)
    nx.draw_networkx_labels(G,g_pos,node_labels_dict,font_size=5)
    if debug:
        plt.show()
    else:
        plt.savefig("comm_{}.png".format(comm_id), format="PNG",dpi=1000)
        
    return

def build_comm_sub_network(comm_id, debug):
    print(">>> Building community sub-network.")

    # make graph object
    G = nx.Graph()

    # obtain nodes for graph
    nodes_list = get_sub_comm_nodes( comm_id )
    #nodes_list = get_comm_nodes( def_nodes, comm_id )
    
    # add nodes to graph
    G.add_nodes_from( [node[0] for node in nodes_list] )

    # obtain edges for graph
    edges_list = get_edges_from_nodes( nodes_list )
    
    # add edges to graph
    G.add_edges_from( [[edge[0], edge[1]] for edge in edges_list] )

    # obtain node attributes
    max_emails = max( [node[1]+node[2] for node in nodes_list] )
    max_emails_sent = max( [node[1] for node in nodes_list] )
    max_emails_rec = max( [node[2] for node in nodes_list] )
    node_colors = get_node_community_colors(nodes_list, sub_comm=True)
    node_sizes = get_node_popularity_sizes(nodes_list, max_emails)
    node_labels_dict = get_node_labels(nodes_list, 0.95, max_emails)
    print("<<< Built community sub-network with {} nodes.".format(len(G.nodes)))

    
    # debug
    print(nx.info(G))

    # centraility metrics
    centrality_metrics(G)

    # draw network
    g_pos = nx.kamada_kawai_layout(G)
    nx.draw(G,pos=g_pos,node_color=node_colors,node_size=node_sizes,width=0.05)
    nx.draw_networkx_labels(G,g_pos,node_labels_dict,font_size=5)
    if debug:
        plt.show()
    else:
        plt.savefig("sub_comm_{}.png".format(comm_id), format="PNG",dpi=1000)
        
    return

def centrality_metrics(G):
    # degree centrality
    degree_centrality = list(nx.degree_centrality(G).values())
    sum_degree = 0
    for val in degree_centrality:
        sum_degree += val
    avg_degree = sum_degree / len(degree_centrality)

    # eigenvector centrality
    eigen_centrality = list(nx.eigenvector_centrality(G).values())
    sum_eigen = 0
    for val in eigen_centrality:
        sum_eigen += val
    avg_eigen = sum_eigen / len(eigen_centrality)

    # closeness centrality
    closeness_centrality = list(nx.closeness_centrality(G).values())
    sum_closeness = 0
    for val in closeness_centrality:
        sum_closeness += val
    avg_closeness = sum_closeness / len(closeness_centrality)

    # betweenness centrality
    betweenness_centrality = list(nx.betweenness_centrality(G).values())
    sum_betweenness = 0
    for val in betweenness_centrality:
        sum_betweenness += val
    avg_betweenness = sum_betweenness / len(betweenness_centrality)

    print("Degree : {}".format(avg_degree))
    print("Eigen : {}".format(avg_eigen))
    print("Closeness : {}".format(avg_closeness))
    print("Betweeness : {}".format(avg_betweenness))

    return


# build_sr_network( 50 )

build_ct_network( None )

# build_comm_network(5, False)

#build_comm_sub_network(0, True)
#build_comm_sub_network(2, True)
#build_comm_sub_network(5, True)

