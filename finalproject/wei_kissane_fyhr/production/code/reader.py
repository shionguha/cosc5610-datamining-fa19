
import pandas as pd

def get_all_emails():
    db_name = "../data/db/PURE_DATABASE.csv"
    print("Reading rows from {}...".format(db_name))
    """
    Returns pandas 'DataFrame' of all emails within csv
    """
    try:
        emails = pd.read_csv(db_name, header=None)
        return emails
    except FileNotFoundError:
        print("< ERROR > : File Not Found : {}.".format(db_name))

    return None



def read_email(arr):

    # get emails
    emails = get_all_emails().values
    print(len(emails))

    for email in emails:
        if email[0] in arr:
            print()
            print("Email : {}.".format(email[0]))
            print()
            print(email[5])

    return

# arr = [32418, 34588, 28819, 33487, 22559, 39213, 26305, 34048, 5637, 24297]
arr = [11945, 19397, 20802, 7350, 17670, 21810, 6746, 17854, 21712, 4742]
# arr = [2, 8, 9, 12, 13, 15, 17, 18, 22, 23]
read_email(arr)
