# functionality common to delite and delitec

import os, sys
import ConfigParser

DELITE_HOME = os.getenv("DELITE_HOME")

scala_major_id = "scala-2.10"
script_path = os.path.dirname(__file__)
# base directory for the project (either DSL or Delite or ?) i.e. drops the '/bin' from script_path
script_home = os.path.split(script_path)[0]

def err(s):
    exit("error: " + s)

def warn(s):
    print("warn: " + s)

def checkDeliteEnv():
    global DELITE_HOME
    if DELITE_HOME is None:
        #try to check if it is in the usual place
        cand_home = script_home + "../../Delite"
        if os.path.isfile(cand_home + "/delite.properties.sample"):
          DELITE_HOME = cand_home
        else:
          err("The DELITE_HOME environment variable must be defined")
