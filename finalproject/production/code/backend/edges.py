"""
This file is responsible for converting database dataset to an edge dataset
[ NODE_A, NODE_B, WEIGHT]
"""
import csv
import pandas as pd
import re


def build_edge_db():
    # Initialize dictionary for edges
    edges = dict()

    # Obtain all emails
    emails = get_emails_from_db()

    # Iterate through emails
    count = 0
    for email in emails:
        count += 1
        sender = str(email[2])
        receivers = str(email[3]).split(",")
        edge_data = [] # [ NODE_A, NODE_B, WEIGHT]
            
        # Update edge data
        for rec in receivers:
            node_a = sender
            node_b = rec
            key_list = [ node_a.lower(), node_b.lower() ]
            key_list.sort()
            key = "-".join(key_list)
            
            val = edges.get(key)
            if val is not None: # key found
                edges[key] = [val[0], val[1], val[2]+1]
                
            else: # key not found
                valid = True
                
                # if sender was also recipient
                if key_list[0] == key_list[1]:
                    valid = False
                # if sender or recipient is unknown
                if key_list[0] == "" or key_list[1] == "":
                    valid = False
                # if sender or recipient is a telephone number
                if ('<' in key_list[0] and '>' in key_list[0] ) or ('<' in key_list[1] and '>' in key_list[1]):
                    valid = False
                if len(re.findall(r"\d{4}\D{1}\d{4}", key_list[0])):
                    valid = False
                if len(re.findall(r"\d{4}\D{1}\d{4}", key_list[1])):
                    valid = False
                # if nan
                if key_list[0] == "nan" or key_list[1] == "nan":
                    valid = False
                # if sender or recipient has no email address
                if not "@" in key_list[0] and "@" in key_list[1]:
                    valid = False

                # if valid new edge
                if valid:
                    edges[key] = [node_a.lower(), node_b.lower(), 1]


        # Progress awareness
        if count % 500 == 0:
            comp_perc = count / float(len(emails))
            comp_perc = comp_perc * 100;
            print("{} % done! {} edges found!".format(comp_perc, len(edges)))

    print("100% done! {} edges found!".format(len(edges)))

    edge_list = []
    values = edges.values()
    for value in values:
        edge_list.append(value)
        
    n = len(edge_list)
    for i in range(0, n):
        for j in range(0, n - i - 1):
            this_elem = edge_list[j]
            next_elem = edge_list[j+1]
            if this_elem[2] < next_elem[2]:
                edge_list[j], edge_list[j+1] = next_elem, this_elem

    write_edges_to_db( edge_list )

def get_emails_from_db( ):
    db_name = "PURE_DATABASE.csv"
    print("Getting emails from {}...".format(db_name))
    """
    """
    try:
        emails = pd.read_csv(db_name)
        return emails.values
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

def write_edges_to_db( edges ):
    print("Writing edges to file...")
    """
    """
    with open('PURE_EDGES.csv', 'a') as csvFile:
        writer = csv.writer( csvFile )
        writer.writerows( edges )

        csvFile.close()

    return True
    
build_edge_db()
