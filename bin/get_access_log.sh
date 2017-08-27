rm -r accesslogs
mkdir -p accesslogs
echo "Copying access logs from server"
scp root@reedoei.com:/var/log/nginx/access* accesslogs/

echo "Unzipping..."
gunzip accesslogs/access.log.*

echo "Separating and geolocating IP addresses..."
python resolve_access_logs.py

