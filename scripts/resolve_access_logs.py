import re
import subprocess
import ast
import sys

import utility
import parse_json

files = utility.get_files('accesslogs/', search_re=r'.*\.log$')

res = {}

for fname in files:
    with open('accesslogs/' + fname) as f:
        contents = f.read()
        
        for line in contents.split('\n'):
            match = re.findall(r'((?:\d{1,3}\.){3}\d{1,3})', line)
            
            if len(match) > 0:
                res[match[0]] = 1

with open('output.txt', 'w') as out:
    for match in res:
        res = subprocess.check_output(['whois', match])
        data = ast.literal_eval(res)

        print(parse_json.handle_format_string('<ip> - <city>, <region>, <    country>', data))

