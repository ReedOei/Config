rm -r accesslogs
mkdir -p accesslogs
scp root@reedoei.com:/var/log/nginx/access* accesslogs/
