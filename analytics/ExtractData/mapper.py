#!/usr/bin/env python3
import sys
import os
sys.path.extend([os.getcwd()])
from mapper_lib.longitudinal_benchmarking_map import LongitudinalBenchmarkingMap
print("init_map", file=sys.stderr)
mapper_object = LongitudinalBenchmarkingMap()
print("run_map", file=sys.stderr)
mapper_object.run()
print("end_map", file=sys.stderr)
