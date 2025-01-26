########################################################################
# Functions used to display messages and debug.
#
# 01/2025 Vincent Labatut
########################################################################
from datetime import datetime




def tlog(offset, msg):
    """Just a function that displays some message in the console, prefixed with the current time.        
        For debugging purposes.

    :param offset (int): offset before the message.
    :param: msg (str): the text to display.
    """  

    # set prefix
    offset_str = "." * offset

    # get current time
    dt_string = datetime.now().strftime("[%d/%m/%Y %H:%M:%S] ")

    # display the whole thing
    print(dt_string + offset_str + msg)
