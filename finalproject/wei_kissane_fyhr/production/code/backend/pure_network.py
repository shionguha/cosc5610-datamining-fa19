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
    db_name = "PURE_COMMS.csv"
    print("Reading rows from {}...".format(db_name))
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
    db_name = "PURE_NODES.csv"
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
    db_name = "PURE_EDGES.csv"
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

def get_node_community_colors(nodes):
    print(">>> Getting node colors from {} nodes.".format(len(nodes)))
    node_colors = list([])

    # Determine node color from community; append to list
    for node in nodes:
        hex_color = "#ffffff"

        if node[3] == 0:
            hex_color = "#ff0000"   # red
        elif node[3] == 1:
            hex_color = "#00ff00"   # lime green
        elif node[3] == 2:
            hex_color = "#0000ff"   # blue
        elif node[3] == 3:
            hex_color = "#ffff00"   # yellow
        elif node[3] == 4:
            hex_color = "#660066"   # purple
        elif node[3] == 5:
            hex_color = "#00ffff"   # cyan
        elif node[3] == 6:
            hex_color = "#ffa500"   # orange
        elif node[3] == 7:
            hex_color = "#ff00ff"   # magenta
        elif node[3] == 8:
            hex_color = "#ffc0cb"   # pink
        elif node[3] == 9:
            hex_color = "#d3ffce"   # seafoam
        elif node[3] == 10:
            hex_color = "#000000"   # dark green
        elif node[3] == 11:
            hex_color = "#003366"   # navy
        elif node[3] == 12:
            hex_color = "#990000"   # brown
        elif node[3] == 13:
            hex_color = "#c0c0c0"   # grey
        elif node[3] == 14:
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
    node_labels_dict = get_node_labels(nodes_list, 0.95)
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



def build_popular_network(n):
    pass
def build_sender_network(n):            # could be important for senders that send to senders
    pass
def build_receiver_network(n):          # receivers that receive from receivers?
    pass


# build_sr_network( 50 )

build_ct_network( 6 )

# build_comm_network(5, False)
