#!usr/bin/python
import sys
import time
import datetime
from datetime import timedelta, date

def get_help():
    print("##HELP")
    print("    This code add time amout (in weeks, days, hours or second) to a date.\n")
    print("#HOW TO USE :")
    print("    python tr_date.py year month day delta unit\n")
    print("#EXAMPLE :")
    print("    Add 2 days to the 2018-03-23 as follow : python tr_date.py 2018 03 23 2 days\n")
    print("#EXAMPLE OUTPUT : ")
    print("    2018-03-23 + 2 days -> 2018-03-25\n")
    print("#HELP END")

def get_in_date(year,month,day):
    in_date=datetime.date(year,month,day)
    return in_date

def tr_date(in_date,delta,unit):
    if unit=="days":
        out_date=in_date+timedelta(days=int(delta))
    elif unit=="weeks":
         out_date=in_date+timedelta(weeks=int(delta))
    elif unit=="hours":
         out_date=in_date+timedelta(hours=int(delta))
    elif unit=="seconds":
         out_date=in_date+timedelta(seconds=int(delta))
    else:
        print("Error, invalid time unit. Choose between years, months, days, hours, seconds.\n")
        sys.exit()
    return out_date

if __name__ == "__main__":
    if len(sys.argv)!=6:
        get_help()
        sys.exit()

    #Get args
    in_year=int(sys.argv[1])
    in_month=int(sys.argv[2])
    in_day=int(sys.argv[3])
    delta=sys.argv[4]
    unit=str(sys.argv[5])

    try:
        in_date=get_in_date(in_year,in_month,in_day) 
        out_date=tr_date(in_date,delta,unit)
        print(str(in_date)+" + "+str(delta)+" "+str(unit)+" ->  "+str(out_date))
    except:
        print("Some error, exit.")
        sys.exit()
