"""
This file is responsible for communications with CSV file
"""
import csv
import pandas as pd
import time
import re

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

def write_clean_emails_to_db( emails ):
    db_name = "PURE_DATABASE.csv"
    print("Writing emails to {}...".format(db_name))
    """
    """
    with open(db_name, 'a') as csvFile:
        writer = csv.writer( csvFile )
        writer.writerows( emails )

        csvFile.close()

    return True

def fix_all_emails( emails ):
    print("Fixing emails : recipients.")
    # The recipients field sometimes has two emails that are not seperated by commas
    fixed_emails = list([])
    for email in emails:
        recs = str(email[3]).split(",")
        
        # fix recs
        good_recs = fix_email_recipients(recs)
        if len(good_recs) > 0:
            for good_rec in good_recs:
                if not "@" in good_rec:
                    print(good_rec)

        email[3] = ",".join(good_recs)
        
        # append email to list
        fixed_emails.append(email)

    return fixed_emails

def fix_email_recipients( recs ):
    fixed = list([])
    for rec in recs:
        if not "@" in rec:
            #fixed.append(rec)
            continue
        
        endings = re.findall("@[a-zA-Z0-9.?/&=:-]+\.\w{3}",rec)
        bodies = re.split("@*?\.\w{3}[m][o][r][e]", rec)
        try:
            if len(bodies) > 1:
                first_ending = re.findall("\.\w{3}", endings[0])
                fixed.append("{}{}".format(bodies[0], first_ending[0]))
                fixed.append("{}".format(bodies[1]))
            else:
                fixed.append(rec)
        except IndexError:
            print(rec)
            print(bodies)
            print(endings)
            return
            
    return fixed



def purify_db():

    # Get emails from CLEAN_DATABASE
    bad_emails = read_clean_emails_from_db()
    print(len(bad_emails))

    # Fix emails' recipients and ccs
    good_emails = fix_all_emails(bad_emails)
    print(len(good_emails))
    print()

    # Write to database
    emails_to_publish = list([])
    pub_count = 0
    last_pub = time.time()
    for email in good_emails:
        # append to publish list
        emails_to_publish.append(email)

        # check if its time to publish
        curr_time = time.time()
        elapsed = curr_time - last_pub
        if len(emails_to_publish) > 1000:
            pub_count += len(emails_to_publish)

            progress = (pub_count) / (len(good_emails)-1)
            print("{}% complete; {} emails published.".format(str(progress*100)[:5],pub_count))

            # Write emails to db
            write_clean_emails_to_db(emails_to_publish)
            emails_to_publish.clear()
            last_save = time.time()
            print()

    if len(emails_to_publish) > 0:
        pub_count += len(emails_to_publish)
            
        print("100% complete; {} emails published.".format(str(pub_count)))

        # Write emails to PURE_DATABASE
        write_clean_emails_to_db(emails_to_publish)
        emails_to_publish.clear()

    return (len(bad_emails) == len(good_emails))

def verify_pure_db():
    db_name = "PURE_DATABASE.csv"
    print("Reading emails from {}...".format(db_name))
    """
    """
    pure_emails = pd.read_csv(db_name).values

    for email in pure_emails:
        recs = str(email[3]).split(",")
        for rec in recs:
            mail_count = len(re.findall("@",rec))
            if mail_count > 1:
                print(email[0])
                print(rec)
                print()

    return
            

"""
if purify_db():
    print ("\nSUCCESS\n")
else:
    print ("\nFAILED\n")
"""
verify_pure_db()
print("DONE!")





        
