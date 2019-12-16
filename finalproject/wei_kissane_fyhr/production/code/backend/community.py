"""
This file is responsible for communications with CSV file
"""
import pandas as pd


def read_clean_emails_from_db():
    db_name = "CLEAN_DATABASE.csv"
    print("Reading emails from {}...".format(db_name))
    """
    """
    try:
        emails = pd.read_csv(db_name)
        return emails.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

def read_comm_nodes_from_db():
    db_name = "COMM_NODES.csv"
    print("Reading nodes from {}...".format(db_name))
    """
    """
    try:
        nodes = pd.read_csv(db_name)
        return nodes.values
    except FileNotFoundError:
        print("< ERROR > : File not found : {}.".format(db_name))

    return None

def get_comm_emails( comm_id ):

    """
    Get Data From Databases
    """
    emails = read_clean_emails_from_db()
    nodes = read_comm_nodes_from_db()
    if emails is None or nodes is None:
        print("< ERROR > : Failed to read file; make sure filename is correct.")
        return None
    
    
    """
    Create Dictionary Of Community Nodes
    """
    comm_nodes = dict({})
    for node in nodes:
        if node[4] == comm_id:
            # matching community
            comm_nodes[node[1].lower()] = node[1]
    
    
    """
    Create List For Emails Within A Community
    """
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



    """
    Return Community Emails As List
    """
    return comm_emails


max_comm_id = 27
for i in range(0, max_comm_id):
    comm_emails = get_comm_emails(i)
    len_comm_emails = len(comm_emails)
    print("Community {} has {} emails.".format(i,len_comm_emails))
