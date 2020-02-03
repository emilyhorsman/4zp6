"""
Take an input file and produce an concatenation of C-style string literals.
"""

with open("include/index.html", "r") as f:
    contents = '\n'.join([
        '"{}"'.format(l.strip().replace('"', '\\"'))
        for l in f.readlines()
    ])

with open("include/index.html._cc", "w") as f:
    f.write(contents)
