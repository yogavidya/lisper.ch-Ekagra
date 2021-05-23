# lisper.ch-Ekagra:
### The full source code for the webapp @https://lisper.ch/Ekagra (online yoga lessons)

# Why?
This is not by any means a showcase of my programming capabilities.
Then, why publish it? (apart from having a free cloud backup ;)
I once wrote on r/lisp that I chose long ago to write all applications as distributed applications with a web frontend.
My main reasons:
* I don't want to deal with the million GUI libraries/toolkits/environments anymore. I can be a better programmer if I don't disperse my energies: so if I switch from Linux to Mac to Windows to whatever, I don't want to be forced to learn the same concepts in a different idiom. So goodbye QT, .NET Razor, Django, CAPI... My applications all have a web frontend as GUI.
* The frontend/backend/storage pattern is a good one even for local applications: a modular architecture allows for better separation of concerns, IOC, dependency injection.

As a consequence, at least a couple of students contacted me with more or less the same question: how does one actually write a web application with a CL backend and an HTML/JS/CSS frontend?

So, in hopes of being useful, this is an example of answer to that question.

# What?
The contents are not really important - it's a webapp I made to publish my Yoga lessons, and it's in Italian. 

It's not exceptionally polished (I did this in a couple of months two years ago, then my private life went totally tits up, and I'm catching up now).

But the point is, it's fully dockerized - a container with a Vue.js frontend, one with a Common Lisp Hunchentoot-based REST server, and one with a Postgres DB. 

It can run locally on a web server (see my local Nginx configuration below) almost out of the box (modulo a couple of text files with passwords for external services). 

It can be used by those who need it as a case study of how to put together the pieces of the big puzzle that is a medium-sized web application.

# How?
Easy, that's the gift of microservices and Docker.
1. git clone
2. add to services/storage a .env file like below:
```
POSTGRES_PASSWORD=[a password]
POSTGRES_USER=ekagra
POSTGRES_DB=ekagra
```
3. add to your local web server a vhost on port 9000: it should act as a reverse proxy for the frontend and the backend.
As an example, below is an extract from my Nginx configuration.
```    
    server {
        listen *:9000;
        server_name lisper.ch;

        #access_log /var/log/nginx/lisperch.access.log;
        #error_log /var/log/nginx/lisperch.error.log;

        try_files $uri $uri/index.html $uri.html =404;

        rewrite /ekagra(.*) $scheme://$host/Ekagra$1 permanent;

        location / {
            root   html;
            index  index.html;
        }
        location /Ekagra {
            proxy_pass http://localhost:8090/;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_redirect off;        
        }
        location ~ /api {
            proxy_pass http://localhost:9999;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_redirect off;        
        }
    }
```

4. docker-compose build
5. docker-compose up
6. browse to http://localhost:9000/Ekagra

For obvious reasons, the content DB is empty and the application won't connect to the SMTP server @lisper.ch.
You can inspect the sources to see how a development and a production build differ, and how to make the same code deployable on a local server or on the actual web host.

Enjoy! Again, in hopes to be useful

Salvatore
