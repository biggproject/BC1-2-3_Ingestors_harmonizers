import sys


class Map(object):
    input_file = sys.stdin

    @staticmethod
    def read_input(file):
        for line in file:
            yield line.strip()

    def finish(self):
        print("finish", file=sys.stderr)

    def run(self):
        print("starting", file=sys.stderr)
        for line in Map.read_input(Map.input_file):
            self.map(line)
        print(f"finished", file=sys.stderr)
        self.finish()

    def map(self, line):
        print(line)
