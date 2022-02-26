FROM erlang:22

ENV APP_DIR="ianisqatsibot"

RUN mkdir $APP_DIR \
    && cd $APP_DIR \
    && git clone https://github.com/igb/ianisqatsibot.git \
    && cd "ianisqatsibot" \
    && rebar get-deps \
    && rebar clean compile 
