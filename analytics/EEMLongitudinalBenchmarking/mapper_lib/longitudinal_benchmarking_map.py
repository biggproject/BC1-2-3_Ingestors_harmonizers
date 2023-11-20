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


class LongitudinalBenchmarkingMap(Map):

    def __init__(self):
        self.shared_folder = "/user/ubuntu/tmp/EEMLongitudinalBenchmarking/shared_input"
        self.timeseries_shared = {}
        with open('building_key.keymap', 'r') as f:
            self.building_keys = json.load(f)
        self.non_building = set()

    def finish(self):
        fs = pyhdfs.HdfsClient(hosts="master1.internal:9870", user_name="ubuntu")
        fs.mkdirs(self.shared_folder)
        for ts_hash, data in self.timeseries_shared.items():
            filename = f"{self.shared_folder}/{ts_hash}.data"
            print(ts_hash, file=sys.stderr)
            try:
                fs.create(filename, "")
            except:
                pass
            fs.append(filename, "\n".join(list(data['data'].values()))+"\n")
            print(data['building_list'], file=sys.stderr)
            for b in data['building_list']:
                print(f"{self.building_keys[b]}\t{pickle.dumps({'building': b, 'file': filename, 'hash': ts_hash, 'meta': 'shared_file'})}")


    def map(self, line):
        ts_hash, start, end, value, is_real = line.split("\t")
        start = pd.to_datetime(float(start), unit="s").tz_localize("UTC").isoformat()
        end = pd.to_datetime(float(end), unit="s").tz_localize("UTC").isoformat()
        is_real = True if is_real == "true" else False
        try:
            value = float(value)
        except:
            pass
        try:
            with open(f"buildings_hash/{ts_hash}.json", "r") as f:
                building_list = json.load(f)[ts_hash]
        except:
            self.non_building.add(ts_hash)
            return
        if len(building_list) > 0:
            # if we have more than 0 buildings, we optimize it by creating a shared file and read it from R directly
            try:
                self.timeseries_shared[ts_hash]['data'][start] = f'{start};{end};{value};{is_real}'
            except:
                self.timeseries_shared[ts_hash] = {
                    'data': {start: f'{start};{end};{value};{is_real}'},
                    'building_list': building_list
                }
            print("reporter:counter: CUSTOM, NbRecords_file,1", file=sys.stderr)
        else:
            self.non_building.add(ts_hash)

if __name__ == "__main__":
    mapper_object = LongitudinalBenchmarkingMap()
    mapper_object.run()
