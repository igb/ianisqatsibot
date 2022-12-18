-module(ianisqatsibot).
-export([get_alt_texts/1,get_previous_tweets/1,init/0,get_tweet_texts/1,run/2,run/3,bootstrap/1,decode_from_file/1,encode_to_file/2,bootstrap_loop/3, loop/2,  mloop/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



init()->
    lists:map(fun(X)->
		      application:start(X) end,
	      [inets, crypto, public_key, ssl]).

%LOOP OFF OF PREVIOUSLY DOWNLOADED ALT TEXTS...NEEDED TO GET AROUND LIMIT OF 3000 TWEETS AND API LIMITS
bootstrap_loop(AltTextSourceScreenName, BotScreenName, FileName) ->
    Alts=decode_from_file(FileName),
    run(AltTextSourceScreenName, BotScreenName, Alts),
    timer:sleep(1000 * 60 * 60 * 6), %DEFAULT INTERVAL OF ~6 HOURS...TODO: BASE THIS OFF OF LAST TWEET TIMESTAMP 
    bootstrap_loop(AltTextSourceScreenName, BotScreenName,FileName).

loop(AltTextSourceScreenName, BotScreenName) ->
    run(AltTextSourceScreenName, BotScreenName),
    timer:sleep(1000 * 60 * 60 * 6), %DEFAULT INTERVAL OF ~6 HOURS...TODO: BASE THIS OFF OF LAST TWEET TIMESTAMP 
    loop(AltTextSourceScreenName, BotScreenName).

%MASTODON ENTRY POINT
mloop(AltTextSourceScreenName, BotScreenName) ->
    mrun(AltTextSourceScreenName, BotScreenName),
    timer:sleep(1000 * 60 * 60 * 6), %DEFAULT INTERVAL OF ~6 HOURS...TODO: BASE THIS OFF OF LAST TWEET TIMESTAMP 
    loop(AltTextSourceScreenName, BotScreenName).



bootstrap(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    TweetIds = read_lines(Device, []),
    Alts=lists:map(fun(X)->  
			   timer:sleep(1000),
			   {Tweet}=erlybird:get_tweet(X, [{"include_ext_alt_text", "true"}]),
			   case lists:keyfind(<<"retweeted_status">>, 1, Tweet) of
			       false ->
				   get_alt_text(Tweet);
			       _ -> false
			   end	  
		   end, TweetIds),
    FilteredAlts=lists:filter(fun(X)->
				      case X of
					  false ->
				 false;
					  [null] ->
					      false;
					  _  ->
					      true
				      end
			      end, Alts),
    FoldedAlts = lists:foldl(fun(X, Acc)-> lists:append(Acc, X) end, [], FilteredAlts),
    
						% the above fold creates new nulls if one or more photos in
						% a single tweet has no alt text so filter again
    lists:map(fun(X) -> 
		      case X of
			  null -> "";
			  _ -> binary_to_list(X)
		      end
	      end, FoldedAlts).

encode_to_file(Alts, File)->
    EncodedAlts =
        lists:map(fun(X) ->
                         base64:encode_to_string(X)
                  end,
                  Alts),
    lists:map(fun(X) ->
		      file:write_file(File,
				      io_lib:fwrite("~s\n", [X]),
				      [append])
              end,
	      EncodedAlts).

decode_from_file(FileName)->
    {ok, Device} = file:open(FileName, [read]),
    EncodedAlts=read_lines(Device, []),
    lists:map(fun(X)->
		      base64:decode_to_string(X)
	      end, EncodedAlts).

read_lines(Device, Acc)->
    case io:get_line(Device, "") of
	eof ->
	    Acc;
	Line ->
	    
	    NewAcc = lists:append(Acc, [string:sub_string(Line, 1, length(Line) - 1 )]),
		read_lines(Device, NewAcc)
    end.
		    
		
         

is_same_or_similar_to_previous_tweet(CandidateTweet, PreviousTweets)->
    io:format("checking ~p~n", [CandidateTweet]),
    SimilarityCheck = lists:foldl(fun(X, Acc) ->
			EditDistance = levenshtein:levenshtein_distance(CandidateTweet, X),
%			io:format("edit distance is ~p~n",[EditDistance]),
			case EditDistance < 10 of
			    true -> Acc + 1;
			    false  -> Acc
			end
		end, 0, PreviousTweets),
    case SimilarityCheck of
	0 ->
	    true;
	_ -> false
    end.
									    
run(AltTextSourceScreenName, BotScreenName)->
    AltTexts=lists:map(fun(X)-> [Y]=X, Y end, get_alt_texts(AltTextSourceScreenName)),
    run(AltTextSourceScreenName, BotScreenName,AltTexts).

run(AltTextSourceScreenName, BotScreenName, AltTexts)->
    PreviousTweets = get_previous_tweets(BotScreenName),
  
    PreviousTweetsTexts = get_tweet_texts(PreviousTweets),
    ReversedAltTexts = lists:reverse(AltTexts),

    FilteredReversedAltTexts = lists:filter(fun(X)->
		 is_same_or_similar_to_previous_tweet(X, PreviousTweetsTexts)	 
		 end, ReversedAltTexts),
    [TweetBody|_] = FilteredReversedAltTexts,
    TweetBody,
    [LastPostedTweet|_]=get_tweet_texts(PreviousTweets),
    io:format("A:~n~s~n", [LastPostedTweet]),
    io:format("B:~n~s~n", [TweetBody]),
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    X=erlybird:post(TweetBody, Consumer, AccessToken, AccessSecret),
    io:format("~p~n", [X]),
    ok.



%MASTODON PATH
mrun(AltTextSourceScreenName, BotScreenName)->
    AltTexts=lists:map(fun(X)-> [Y]=X, Y end, get_alt_texts(AltTextSourceScreenName)),
    mrun(AltTextSourceScreenName, BotScreenName,AltTexts).


%MASTODON PATH
mrun(AltTextSourceScreenName, BotScreenName, AltTexts)->
    PreviousTweets = get_previous_tweets(BotScreenName),
  
    PreviousTweetsTexts = get_tweet_texts(PreviousTweets),
    ReversedAltTexts = lists:reverse(AltTexts),

    FilteredReversedAltTexts = lists:filter(fun(X)->
		 is_same_or_similar_to_previous_tweet(X, PreviousTweetsTexts)	 
		 end, ReversedAltTexts),
    [TweetBody|_] = FilteredReversedAltTexts,
    TweetBody,
    [LastPostedTweet|_]=get_tweet_texts(PreviousTweets),
    io:format("A:~n~s~n", [LastPostedTweet]),
    io:format("B:~n~s~n", [TweetBody]),
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    X=erlybird:post(TweetBody, Consumer, AccessToken, AccessSecret),
    io:format("~p~n", [X]),
    ok.



get_previous_tweets(ScreenName)->
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    Timeline = erlybird:get_entire_timeline([{count, "200"},{include_ext_alt_text, "true"}, {screen_name, ScreenName}, {include_rts, "false"}], Consumer, AccessToken, AccessSecret),
    Timeline.

get_tweet_texts(Tweets)->
    lists:map(fun(X)->  
		      {Tweet} = X,
		      {<<"text">>, Text} = lists:keyfind(<<"text">>, 1, Tweet),
		      ReplacedText = unescape_html(Text),
		      replace_urls(extract_urls(Tweet), ReplacedText)
	      end, Tweets).


unescape_html(Text)->
    RegExes = [{"\\&amp;", "\\&"},
     {"\\&lt;", "<"}],
    unescape_html(Text, RegExes).
    
unescape_html(Text, [H|T])->
    {Expression, Replacement}=H,
    {ok, Mp}=re:compile(Expression),
    ReplacedText = re:replace(Text, Mp, Replacement, [{return, list}, global]),
    unescape_html(ReplacedText, T);
unescape_html(Text, []) ->
    Text.

extract_urls(Tweet)->
    {<<"entities">>, {Entities}} = lists:keyfind(<<"entities">>, 1, Tweet),
    {<<"urls">>, Urls} = lists:keyfind(<<"urls">>, 1, Entities),
    extract_urls(Urls, []).


extract_urls([H|T], Acc)-> 
    {[{<<"url">>,Url},
      {<<"expanded_url">>,ExpandedUrl},
      {<<"display_url">>,_},
      {<<"indices">>,_}]}=H,
    NewAcc = lists:append(Acc, [{binary_to_list(Url), binary_to_list(ExpandedUrl)}]),
    extract_urls(T, NewAcc);
extract_urls([], NewAcc) ->
    NewAcc.


replace_urls([Urls|T], UnreplacedUrlText)->
    {Old, New}=Urls,
    {ok, Mp}=re:compile(Old),
   
    % check for .app "URL" mis-encoding...
    case string:sub_string(New, length(New) - 3) of
	".app" ->
	    CleanUrl = string:sub_string(New, 8, length(New));
	_-> CleanUrl = New
    end,
    ReplacedUrlText = re:replace(UnreplacedUrlText, Mp, CleanUrl, [{return, list}]),    
    
    replace_urls(T, ReplacedUrlText);
replace_urls([],UnreplacedUrlText) ->
    UnreplacedUrlText.
    


     
get_alt_text(Tweet)->
    case lists:keyfind(<<"extended_entities">>, 1, Tweet) of
	false ->
	    false;
	{<<"extended_entities">>, {ExtendedEntities}} ->
	    io:format("~p~n", [ExtendedEntities]), 
	    case lists:keyfind(<<"media">>, 1, ExtendedEntities) of
		false -> 
		    false;
		{<<"media">>, Items} ->
		    lists:map(fun(Y)->
				      {Item} = Y,
				      case lists:keyfind(<<"ext_alt_text">>, 1, Item) of
					  false ->
					      false;
					  {<<"ext_alt_text">>, AltText} ->
					      AltText
				      end
			      end, Items)
	    end
		
    end.

get_alt_texts(ScreenName)->
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    Timeline = erlybird:get_entire_timeline([{count, "200"},{include_ext_alt_text, "true"}, {screen_name, ScreenName}, {include_rts, "false"}], Consumer, AccessToken, AccessSecret, 15000),
    AltTexts = lists:map(fun(X)-> {Y}=X,get_alt_text(Y) end, Timeline),
    FilteredAltTexts = lists:filter(fun(X) -> case X of
						  false ->
						      false;
						  null ->
						      false;
						  _ ->
						      true
					      end
				    end, lists:flatten(AltTexts)),
    
    lists:map(fun(X)-> io_lib:format("~s", [X]) end, lists:filter(fun(X)->
									  iolist_size(X) =< 140
								  end, FilteredAltTexts)).
    
    



-ifdef(TEST).
unescape_html_test()->
    EscapedHtmlText = "Hello &amp; world &lt; Hello &amp;&amp; world&lt;&lt;.",
    UnescapedHtmlText = unescape_html(EscapedHtmlText),
    ExpectedHtmlText = "Hello & world < Hello && world<<.",
    io:format("~p~n", [UnescapedHtmlText]),
    ?assert(UnescapedHtmlText =:= ExpectedHtmlText).

url_replacement_test()->
    ExpectedUrlText = "A screenshot displaying at least four, stacked Safari pop-ups, each reading: 'From \"https://calendar.google.com\":'.",
    UnreplacedUrlText = "A screenshot displaying at least four, stacked Safari pop-ups, each reading: 'From \"https://t.co/pHNV2XySXF\":'.",
    ReplacedUrlText = replace_urls([{"https://t.co/pHNV2XySXF","https://calendar.google.com"}], UnreplacedUrlText),
    ?assert(ReplacedUrlText =:= ExpectedUrlText).

url_extraction_test()->
    Tweet = {[{<<"created_at">>,<<"Tue Aug 22 15:02:46 +0000 2017">>},
  {<<"id">>,900010363797688320},
  {<<"id_str">>,<<"900010363797688320">>},
  {<<"text">>,
   <<"A screenshot displaying at least four, stacked Safari pop-ups, each reading: 'From \"https://t.co/pHNV2XySXF\":'.">>},
  {<<"truncated">>,false},
  {<<"entities">>,
   {[{<<"hashtags">>,[]},
     {<<"symbols">>,[]},
     {<<"user_mentions">>,[]},
     {<<"urls">>,
      [{[{<<"url">>,<<"https://t.co/pHNV2XySXF">>},
         {<<"expanded_url">>,<<"https://calendar.google.com">>},
         {<<"display_url">>,<<"calendar.google.com">>},
         {<<"indices">>,"Tk"}]}]}]}},
  {<<"source">>,
   <<"<a href=\"http://hccp.org\" rel=\"nofollow\">ianisqatsibot</a>">>},
  {<<"in_reply_to_status_id">>,null},
  {<<"in_reply_to_status_id_str">>,null},
  {<<"in_reply_to_user_id">>,null},
  {<<"in_reply_to_user_id_str">>,null},
  {<<"in_reply_to_screen_name">>,null},
  {<<"user">>,
   {[{<<"id">>,767746267833180161},
     {<<"id_str">>,<<"767746267833180161">>},
     {<<"name">>,<<"IANISQATSI">>},
     {<<"screen_name">>,<<"ianisqatsibot">>},
     {<<"location">>,<<"San Francisco, CA">>},
     {<<"description">>,
      <<"This is a bot that tweets the alt-text from images contained in @igb's tweets.  Inspired by the awesome @__koyaanisqatsi.">>},
     {<<"url">>,null},
     {<<"entities">>,{[{<<"description">>,{[{<<"urls">>,[]}]}}]}},
     {<<"protected">>,false},
     {<<"followers_count">>,18},
     {<<"friends_count">>,5},
     {<<"listed_count">>,0},
     {<<"created_at">>,<<"Mon Aug 22 15:32:29 +0000 2016">>},
     {<<"favourites_count">>,11},
     {<<"utc_offset">>,null},
     {<<"time_zone">>,null},
     {<<"geo_enabled">>,false},
     {<<"verified">>,false},
     {<<"statuses_count">>,267},
     {<<"lang">>,<<"en">>},
     {<<"contributors_enabled">>,false},
     {<<"is_translator">>,false},
     {<<"is_translation_enabled">>,false},
     {<<"profile_background_color">>,<<"000000">>},
     {<<"profile_background_image_url">>,
      <<"http://abs.twimg.com/images/themes/theme1/bg.png">>},
     {<<"profile_background_image_url_https">>,
      <<"https://abs.twimg.com/images/themes/theme1/bg.png">>},
     {<<"profile_background_tile">>,false},
     {<<"profile_image_url">>,
      <<"http://pbs.twimg.com/profile_images/769070033804922883/f5pW9a0q_normal.jpg">>},
     {<<"profile_image_url_https">>,
      <<"https://pbs.twimg.com/profile_images/769070033804922883/f5pW9a0q_normal.jpg">>},
     {<<"profile_banner_url">>,
      <<"https://pbs.twimg.com/profile_banners/767746267833180161/1472194390">>},
     {<<"profile_link_color">>,<<"E81C4F">>},
     {<<"profile_sidebar_border_color">>,<<"000000">>},
     {<<"profile_sidebar_fill_color">>,<<"000000">>},
     {<<"profile_text_color">>,<<"000000">>},
     {<<"profile_use_background_image">>,false},
     {<<"has_extended_profile">>,false},
     {<<"default_profile">>,false},
     {<<"default_profile_image">>,false},
     {<<"following">>,false},
     {<<"follow_request_sent">>,false},
     {<<"notifications">>,false},
     {<<"translator_type">>,<<"none">>}]}},
  {<<"geo">>,null},
  {<<"coordinates">>,null},
  {<<"place">>,null},
  {<<"contributors">>,null},
  {<<"is_quote_status">>,false},
  {<<"retweet_count">>,0},
  {<<"favorite_count">>,0},
  {<<"favorited">>,false},
  {<<"retweeted">>,false},
  {<<"possibly_sensitive">>,false},
  {<<"possibly_sensitive_appealable">>,false},
	      {<<"lang">>,<<"en">>}]},
    {TweetBody}=Tweet,
    ?assert(extract_urls(TweetBody)  =:= [{"https://t.co/pHNV2XySXF","https://calendar.google.com"}]).


-endif.
