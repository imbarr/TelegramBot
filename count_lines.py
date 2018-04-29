import sys
from os.path import dirname, basename, join, isfile, isdir
from os import listdir

def listdir_fullpath(d):
    return [join(d, f) for f in listdir(d)]

def count_lines(file, ext):
    if isfile(file) and file.endswith(ext):
        with open(file, "r", encoding='utf8') as f:
            return len([x for x in f.readlines() if x])
    elif isdir(file):
        return sum(count_lines(x, ext) for x in listdir_fullpath(file))
    return 0
        
print(count_lines(sys.argv[1],
    ("" if sys.argv[2].startswith(".") else ".") + sys.argv[2]))