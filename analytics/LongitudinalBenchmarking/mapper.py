#!/usr/bin/env python3
import sys
import os
print(f"loading_cwd {os.getcwd()}", file=sys.stderr)
sys.path.extend([os.getcwd()])
from mapper_lib.longitudinal_benchmarking_map import LongitudinalBenchmarkingMap
print("hola", file=sys.stderr)
print("init_map", file=sys.stderr)
mapper_object = LongitudinalBenchmarkingMap()
print("run_map", file=sys.stderr)
mapper_object.run()
