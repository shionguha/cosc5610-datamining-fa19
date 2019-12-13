"""
This file is responsible for communications with CSV file
"""
import csv
import pandas as pd
import re


def build_node_db():
    # Initialize dictionary for nodes
    nodes = dict()

    # Obtain all emails
    emails = get_emails_from_db()

    # Iterate through emails
    count = 0.0
    for email in emails:
        count += 1.0
        sender = str(email[2])
        receivers = str(email[3]).split(",")
        node_data = [] # [sender_id, emails_sent, emails_received]

        # Update sender data
        key = sender.lower()
        val = nodes.get(key)
        if val is not None: # key found
            nodes[key] = [val[0], val[1]+1, val[2]]
            
        else: # key not found
            valid = True
            
            # if sender is unknown
            if sender == "":
                valid = False
            # if sender is a telephone number
            if ('<' in sender and '>' in sender):
                valid = False
            if len(re.findall(r"\d{4}\D{1}\d{4}", sender)):
                valid = False
            # if sender or recipient has no email address
            if not "@" in sender:
                valid = False

            # if valid new edge
            if valid:
                nodes[key] = [sender.lower(), 1, 0]
            
        # Update receiver data
        for rec in receivers:
            valid = True
            
            # if sender is unknown
            if rec == "":
                valid = False
            # if sender is a telephone number
            if ('<' in rec and '>' in rec):
                valid = False
            if len(re.findall(r"\d{4}\D{1}\d{4}", rec)):
                valid = False
            # if sender or recipient has no email address
            if not "@" in rec:
                valid = False

            # if receiver not valid
            if not valid:
                continue
            
            key = rec.lower()
            val = nodes.get(key)
            if val is not None: # key found
                nodes[key] = [val[0], val[1], val[2]+1]
            else: # key not found
                nodes[key] = [rec.lower(), 0, 1]

        # Progress awareness
        if count % 500 == 0:
            comp_perc = count / float(len(emails))
            comp_perc = comp_perc * 100;
            print("{}% done! {} nodes found!".format(comp_perc, len(nodes)))

    print("100% done! {} nodes found!".format(len(nodes)))

    write_nodes_to_db( nodes.values() )

def get_emails_from_db( ):
    db_name = "PURE_DATABASE.csv"
    print("Reading emails from {}...".format(db_name))
    """
    """
    try:
        emails = pd.read_csv(db_name)
        return emails.values
    
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None

def write_nodes_to_db( nodes ):
    print("Writing nodes to file...")
    """
    """
    with open('PURE_NODES.csv', 'a') as csvFile:
        writer = csv.writer( csvFile )
        writer.writerows( nodes )

        csvFile.close()
        
    return True

def test_db(start, end):
    rows = get_emails_from_db()

    count = 0
    last_id = 0 
    for row in rows[start:]:
        this_id = int(row[0])
                  
        if this_id - last_id > 1:
            print("Found Error : {}".format(last_id+1))
            break

        last_id = this_id
        count += 1

    print("Done")
    return          
    
build_node_db()
print("Done")

