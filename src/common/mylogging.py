########################################################################
# Functions used to display messages and debug.
#
# 01/2025 Vincent Labatut
########################################################################
from datetime import datetime
from os import path




########################################################################
FOLDER_LOG = "log"




########################################################################
# start time of the log
START_TIME = datetime.now()
# no opened log file
CONNECTION = None




########################################################################
def start_rec_log(name = None):
    """Start recording the logs in a text file.

    :param text (str): main name of the log file.
    """

    global START_TIME, CONNECTION

    START_TIME = datetime.now()
	
    prefix = START_TIME.strftime("%Y%m%d_%H%M%S")
    log_file = path.join(FOLDER_LOG, prefix)
    if not name is None:
        log_file = log_file + "_" + name
    log_file = log_file + ".txt"
	
    CONNECTION = open(log_file, "w", encoding = "utf-8")




########################################################################
def tlog(offset, msg):
    """Displays some message in the console, prefixed with the current time.        
        For debugging purposes.

    :param offset (int): offset before the message.
    :param: msg (str): the text to display.
    """

    global CONNECTION

    # set prefix
    offset_str = "." * offset

    # get current time
    dt_string = datetime.now().strftime("[%d/%m/%Y %H:%M:%S] ")

    # display the whole thing
    text = dt_string + offset_str + msg
    print(text)

    # possibly write in log file
    if not CONNECTION is None:
        text = text + "\n"
        CONNECTION.write(text)
        CONNECTION.flush()




#############################################################################################
def end_rec_log():
    """Stops recording the logs in a text file.
    """

    global START_TIME, CONNECTION

    end_time = datetime.now()
    duration = end_time - START_TIME
    tlog(0, "Total processing time: " + str(duration))
    CONNECTION.close()




#############################################################################################
# # test
# start_rec_log("test")
# tlog(2, "risdoifdsniokndsv")
# tlog(4, "yjdghrzgdfhd")
# tlog(2, "poibn xcvserzdwcv")
# end_rec_log()
