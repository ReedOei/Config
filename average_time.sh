echo $2
python -m timeit -n 1 -r "$1" -s 'import os' "os.system('$2')"
