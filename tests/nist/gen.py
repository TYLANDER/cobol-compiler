import os
d = "/Users/tylerschmidt/Projects/cobol-compiler/tests/nist"
Q = chr(34)

def w(name, content):
    with open(os.path.join(d, name), "w") as f:
        f.write(content)
    print(f"{name} written ({len(content)} bytes)")

