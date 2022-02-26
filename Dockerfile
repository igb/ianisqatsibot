FROM erlang:22

ENV APP_DIR="ianisqatsibot"

RUN mkdir $APP_DIR \
    && cd $APP_DIR \
    && git clone https://github.com/igb/ianisqatsibot.git \
    && cd "ianisqatsibot" \
    && rebar get-deps \
    && rebar clean compile 




ENTRYPOINT cd $APP_DIR/ianisqatsibot \
       && erl -pa ./ebin/ -pa ./deps/erlybird/ebin/ -pa ./deps/jiffy/ebin/ -pa  ./deps/oauth/ebin/ -pa  ./deps/levenshtein/ebin/ -s inets  -s ssl -consumer_key "$CONSUMER_KEY" -consumer_secret "$CONSUMER_SECRET" -access_token "$ACCESS_TOKEN" -access_token_secret "$ACCESS_TOKEN_SECRET" -eval 'ianisqatsibot:loop("igb", "ianisqatsibot").' -noshell & echo "deployed!"

