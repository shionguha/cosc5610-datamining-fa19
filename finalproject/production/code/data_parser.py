"""
This file is responsible for accessing locally stored data (csv files)
"""

import pandas as pd

"""#############################################################################################"""
"""
                                        EMAIL DATABASE
"""
"""#############################################################################################"""
# ALL
def get_all_emails():
    db_name = "../data/db/PURE_DATABASE.csv"
    print("Reading rows from {}...".format(db_name))
    """
    Returns pandas 'DataFrame' of all emails within csv
    """
    try:
        emails = pd.read_csv(db_name)
        return emails
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

# COMMUNITY
def get_community_emails( comm_id ):

    # Get Data From Databases
    emails = get_all_emails().values
    nodes = get_all_nodes().values
    if emails is None or nodes is None:
        print("< ERROR > : Failed to read file; make sure filename is correct.")
        return None

    # Create Dictionary Of Community Nodes
    comm_nodes = dict({})
    for node in nodes:
        if node[3] == comm_id:
            # matching community
            comm_nodes[node[0].lower()] = node[0]

    # Create List For Emails Within A Community
    comm_emails = list([])
    for email in emails:
        sender = str(email[2])
        recipients = str(email[3]).split(",")

        if len(recipients) < 1:
            continue

        # check if sender is in community
        if comm_nodes.get(sender.lower()) is not None:
            # check if a recipient is in the community
            for recipient in recipients:
                if comm_nodes.get(recipient.lower()) is not None:
                    # email belongs in community
                    comm_emails.append(email)
                    break

    # Return Community Emails As List
    return comm_emails

# SUB-COMMUNITY
def get_sub_community_emails(comm_id, sub_comm_id):
    # Get Data From Databases
    emails = get_all_emails().values
    nodes = get_community_nodes(comm_id).values
    if emails is None or nodes is None:
        print("< ERROR > : Failed to read file; make sure filename is correct.")
        return None

    # Create Dictionary Of Community Nodes
    comm_nodes = dict({})
    for node in nodes:
        if node[3] == comm_id:
            # matching community
            if node[4] == sub_comm_id:
                # matching sub-community
                comm_nodes[node[0].lower()] = node[0]

    # Create List For Emails Within A Community
    comm_emails = list([])
    for email in emails:
        sender = str(email[2])
        recipients = str(email[3]).split(",")

        if len(recipients) < 1:
            continue

        # check if sender is in community
        if comm_nodes.get(sender.lower()) is not None:
            # check if a recipient is in the community
            for recipient in recipients:
                if comm_nodes.get(recipient.lower()) is not None:
                    # email belongs in community
                    comm_emails.append(email)
                    break

    # Return Community Emails As List
    return comm_emails

# NODE
def get_node_emails(node_id):
    pass
# EDGE
def get_edge_emails(node_id_a, node_id_b):
    pass


"""#############################################################################################"""
"""
                                        NODES DATABASE
"""
"""#############################################################################################"""
# ALL
def get_all_nodes():
    db_name = "../data/db/PURE_COMM_NODES.csv"
    print("Reading rows from {}...".format(db_name))
    """
    Returns pandas 'DataFrame' of all nodes within csv
    """
    try:
        nodes = pd.read_csv(db_name)
        return nodes
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

# COMMUNITY
def get_community_nodes(comm_id):
    db_name = "../data/misc/COMM_{}_SUB_COMMS.csv".format(comm_id)
    print("Reading rows from {}...".format(db_name))
    """
    Returns pandas 'DataFrame' of community nodes within specified csv
    """
    try:
        nodes = pd.read_csv(db_name)
        return nodes
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

# SUB-COMMUNITY
def get_sub_community_nodes():
    pass
# EDGES
def get_nodes_from_edges():
    pass

"""#############################################################################################"""
"""
                                        EDGES DATABASE
"""
"""#############################################################################################"""
# ALL

# COMMUNITY

# NODES
