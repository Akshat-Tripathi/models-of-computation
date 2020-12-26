#A python file to link several lisp files together into a .ros file
#The only reason this exists is because there's an error with my ros build
# which doesn't let me load other lisp files in

import sys

parsed = {}

def process_lib(line: str):
    global libs
    global pkgs
    if line.startswith("(ql"):
        if line not in libs:
            libs += line
        return ""
    elif line.startswith("(use-package"):
        if line not in pkgs:
            pkgs += line
        return ""
    return line

def process_load(line: str):
    if line.startswith("(load"):
        return parse_file(line.split(" ")[1].replace("\"", ""))
    return line

def parse_file(filename :str):
    if filename not in parsed:
        print(filename)
        parsed[filename] = True
        with open(filename, "r") as file:
            return "".join(process_lib(process_load(line)) for line in file.readlines())
    return ""

if __name__ == "__main__":
    dest = sys.argv[1]
    src  = sys.argv[2]
    
    libs = ""
    pkgs = ""

    txt = parse_file(src)
    with open(dest, "r") as file:
        old = file.read()

    with open(dest, "w") as file:
        new = old.replace("#start", "".join([libs, pkgs, txt]))
        file.write(new)
