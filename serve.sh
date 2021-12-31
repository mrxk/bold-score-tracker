#!/bin/bash
docker run --restart=unless-stopped -it -d -p 3001:80 --name bold-counter-web -v $(pwd):/usr/share/nginx/html nginx
