import ast
import re
import sys

def handle_format_string(format_string, data):
    def parse_keyexprs(s):
        res = []
        level = 0
        start = 0 
        for i, c in enumerate(s):
            if c == '<':
                if level == 0:
                    start = i
                level += 1
            elif c == '>':
                level -= 1

                if level == 0:
                    end = i
                    
                    keyexpr = s[start + 1:end]
                    res.append(keyexpr)
        return res

    all_keyexprs = parse_keyexprs(format_string)

    def handle_keyexpr(keyexpr, data):
        if ':' in keyexpr:
            params = keyexpr.split(':')
            keyexpr = params[0]
            rest = ':'.join(params[1:])

            for key in data:
                if re.findall(keyexpr, key) != []:
                    return handle_format_string(rest, data[key])
        else:
            for key in data:
                if re.findall(keyexpr, key) != []:
                    return data[key]

    for keyexpr in all_keyexprs:
        value = handle_keyexpr(keyexpr, data)
        
        format_string = format_string.replace('<{}>'.format(keyexpr), str(value))

    return format_string

if __name__ == '__main__':
    format_string = sys.argv[1]

    if sys.stdin.isatty() and (len(sys.argv) < 2 or format_string == '--help'):
        print('Please pass in a format string.')
        print('The string should contain sections of <keyexpr>.')
        print('For which keys you want to display.')
        print('Key expressions are regular expressions.')
        print('Any key matching the regular expression will be used, but only the first one.')
        print('If you wish to display nested keys, do it as follows:')
        print('<keyexpr:More stuff <keyexpr>>')
        exit()

    value = ''
    for line in sys.stdin:
        value += line
        print(line)

    print(handle_format_string(format_string, ast.literal_eval(value)))

