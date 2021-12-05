from collections import defaultdict
import re
import sys

if len(sys.argv) != 2:
    sys.stderr.write(f'Usage: {sys.argv[0]} <input>\n')
    sys.exit(1)

regex = re.compile(r'->|,')
def parse_line(line):
    return [int(x) for x in regex.split(line)]

def sign(x):
    return 1 if x > 0 else -1

def points_along(segment):
    x1, y1, x2, y2 = segment
    if x1 == x2:
        return ((x1, y) for y in range(min(y1, y2),max(y1, y2)+1))
    elif y1 == y2:
        return ((x, y1) for x in range(min(x1, x2),max(x1, x2)+1))
    return []


with open(sys.argv[1]) as f:
    segments =  [parse_line(line) for line in f]

hits = defaultdict(int)
for segment in segments:
    for point in points_along(segment):
        hits[point] += 1

count = 0
for value in hits.values():
    if value > 1:
        count += 1
print(count)