#!/usr/bin/env python3
import subprocess
import sys
import os
import glob
import rdflib
import pandas as pd
import pickle
import time
import json
from multiprocessing import Process
import pyhdfs
from mapper_lib.Map import Map
import tarfile

class LongitudinalBenchmarkingMap(Map):

    def __init__(self):
        self.extraction_folder = "/user/ubuntu/tmp/Extract/data/"
        self.timeseries_shared = {}

    def finish(self):
        fs = pyhdfs.HdfsClient(hosts="master1.internal:9870", user_name="ubuntu")
        fs.mkdirs(self.extraction_folder)
        for ttl_f in glob.glob(f"*.ttl"):
            if not fs.exists(f"{self.extraction_folder}{ttl_f}"):
                fs.copy_from_local(ttl_f,f"{self.extraction_folder}{ttl_f}")
        for ts_hash, data in self.timeseries_shared.items():
            filename = f"{self.extraction_folder}{ts_hash}.json"
            print(ts_hash, file=sys.stderr)
            try:
                fs.create(filename, "")
            except:
                pass
            fs.append(filename, f"{{\"{ts_hash}\": [".encode() + ", ".join(list(data['data'].values())).encode()+"]}".encode())

    def map(self, line):
        ts_hash, start, end, value, is_real = line.split("\t")
        start = pd.to_datetime(float(start), unit="s").tz_localize("UTC").isoformat()
        end = pd.to_datetime(float(end), unit="s").tz_localize("UTC").isoformat()
        try:
            value = float(value)
        except:
            pass
        try:
            with open(f"buildings_hash/{ts_hash}.json", "r") as f:
                building_list = json.load(f)[ts_hash]
        except:
            return
        if len(building_list) > 0:
            # if we have more than 0 buildings, we optimize it by creating a shared file and read it from R directly
            try:
                self.timeseries_shared[ts_hash]['data'][start] = f'{{"start": "{start}", "end": "{end}", "value": {value}, "isReal": {is_real}}}'
            except:
                self.timeseries_shared[ts_hash] = {
                    'data': {start: f'{{"start": "{start}", "end": "{end}", "value": {value}, "isReal": {is_real}}}'},
                    'building_list': building_list
                }
            print("reporter:counter: CUSTOM, NbRecords_file,1", file=sys.stderr)

if __name__ == "__main__":
    mapper_object = LongitudinalBenchmarkingMap()
    mapper_object.run()
