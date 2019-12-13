"""
This file is responsible for decomposing a network into clusters
"""

import networkx as nx
import pandas as pd
import community
import csv

import network as mynetwork

def decompose_full_network():
    """
    Decompose network into communities, write node list to new CSV
    """
    print(">>> Decomposing Full Network Into Communities...")
    file_name = "../data/misc/NETWORK_COMMS.csv"
    
    # Bring In Network Edges & Nodes
    nodes_df = pd.read_csv("../data/db/PURE_NODES.csv", header=None)
    edges_df = pd.read_csv("../data/db/PURE_EDGES.csv", header=None)

    # Rename Columns For Nodes & Edges
    nodes_df = nodes_df.rename(columns={0:'EMAIL_ID', 1:'EMAILS_SENT',2:'EMAILS_RECIEVED'})
    edges_df = edges_df.rename(columns={0:'NODE_A',1:'NODE_B',2:'WEIGHT'})

    # Create Network
    G = nx.from_pandas_edgelist(edges_df,'NODE_A','NODE_B')

    # Decompose Network
    partition = community.best_partition(G)

    # Obtain Nodes
    comm_nodes = [partition.get(node) for node in G.nodes()]

    # Create Row Data
    communities_df = pd.DataFrame(list(partition.items()), columns=['EMAIL_ID','COMM_ID'])
    communities_df = pd.merge(nodes_df, communities_df, on="EMAIL_ID")

    # Write Nodes To CSV
    print("<<< ...Done Decomposing Full Network!")
    communities_df.to_csv(file_name, index=False)
    

def decompose_community(comm_id):
    """
    Decompose community into sub-communities, write node list to new CSV
    """
    print(">>> Decomposing Community Into Sub-Communities...")
    file_name = "../data/misc/COMM_{}_SUB_COMMS.csv".format(comm_id)

    # Bring In Community Edges & Nodes
    all_comm_nodes_df = pd.read_csv("../data/misc/NETWORK_COMMS.csv")
    comm_nodes_df = all_comm_nodes_df.loc[all_comm_nodes_df['COMM_ID'] == comm_id]

    comm_edges = mynetwork.get_edges_from_nodes(comm_nodes_df.values)
    comm_edges_df = pd.DataFrame(comm_edges)

    # Rename Columns For Nodes & Edges
    comm_nodes_df = comm_nodes_df.rename(columns={0:'EMAIL_ID', 1:'EMAILS_SENT',2:'EMAILS_RECIEVED',3:'COMM_ID'})
    comm_edges_df = comm_edges_df.rename(columns={0:'NODE_A',1:'NODE_B',2:'WEIGHT'})

    # Create Network
    G = nx.from_pandas_edgelist(comm_edges_df,'NODE_A','NODE_B')

    # Decompose Network
    partition = community.best_partition(G)

    # Obtain Nodes
    comm_nodes = [partition.get(node) for node in G.nodes()]

    # Create Row Data
    community_df = pd.DataFrame(list(partition.items()), columns=['EMAIL_ID','SUB_COMM_ID'])
    community_df = pd.merge(comm_nodes_df, community_df, on="EMAIL_ID")

    # Write Nodes To CSV
    print(">>> Done Decomposing Community {}...".format(comm_id))
    community_df.to_csv(file_name, index=False)


