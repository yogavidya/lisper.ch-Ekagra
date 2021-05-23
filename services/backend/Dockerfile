FROM alpine as creation_stage
WORKDIR /root
RUN apk add sbcl
RUN apk add git
RUN apk add emacs-nox
RUN apk add mc
RUN git clone https://github.com/slime/slime.git
COPY ./requirements/.emacs .
COPY ./requirements/install-quicklisp.lisp .
COPY ./requirements/local-projects ./quicklisp/local-projects/
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load "install-quicklisp" --quit
COPY ./requirements/.sbclrc .
RUN sbcl     --eval "(ql:quickload :swank)" \
    --eval "(ql:quickload :cl-postgres)" \
    --eval "(ql:quickload :flexi-streams)" \
    --eval "(ql:quickload :cl-interpol)" \
    --eval "(ql:quickload :hunchentoot)" \
    --quit
EXPOSE 9999/tcp
# Backend first compilation: select mode (dev/prod) here
ENTRYPOINT exec sbcl \
    --eval \
    "(when (not (equalp (machine-instance) \"lisperch-backend-development\")) (pushnew :EKAGRA-PRODUCTION *features*) (print \"production backend\"))" \
    --eval "(ql:quickload :lisper-backend)" \
    --eval "(lisper-backend:start-server)"



