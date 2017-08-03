-module(ianisqatsibot).
-export([get_alt_texts/1,get_previous_tweets/1,init/0,get_tweet_texts/1,run/2,run/3,bootstrap/1,decode_from_file/1,encode_to_file/2,bootstrap_loop/3]).


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

bootstrap(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    TweetIds = read_lines(Device, []),
    Alts=lists:map(fun(X)->  
			   io:format("~p~n", [X]),
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
		    
		
         


run(AltTextSourceScreenName, BotScreenName)->
    AltTexts=lists:map(fun(X)-> [Y]=X, Y end, get_alt_texts(AltTextSourceScreenName)),
    run(AltTextSourceScreenName, BotScreenName,AltTexts).

run(AltTextSourceScreenName, BotScreenName, AltTexts)->
    PreviousTweets = get_previous_tweets(BotScreenName),
  
    PreviousTweetsSet = sets:from_list(get_tweet_texts(PreviousTweets)),
  
   
    
    ReversedAltTexts = lists:reverse(AltTexts),
    FilteredReversedAltTexts = lists:filter(fun(X)->
			 not sets:is_element(X, PreviousTweetsSet)
		 end, ReversedAltTexts),
    [TweetBody|_] = FilteredReversedAltTexts,
    TweetBody,
    io:format("~n~s~n", [TweetBody]),
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    erlybird:post(TweetBody, Consumer, AccessToken, AccessSecret).


get_previous_tweets(ScreenName)->
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    Timeline = erlybird:get_entire_timeline([{count, "200"},{include_ext_alt_text, "true"}, {screen_name, ScreenName}, {include_rts, "false"}], Consumer, AccessToken, AccessSecret),
    Timeline.

get_tweet_texts(Tweets)->
    lists:map(fun(X)->  
		      {Tweet} = X,
		      {<<"text">>, Text} = lists:keyfind(<<"text">>, 1, Tweet),
		      
		      % sanitize &amp;
		      
		      



		      NonBinaryText = binary_to_list(Text),
		      {ok, Mp}=re:compile("\\&amp;"),
		      ReplacedText = re:replace(NonBinaryText, Mp, "\\&", [{return, list}]),
		      ReplacedText
		      

			  
			  

	      end, Tweets).

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
    AltTexts = lists:map(fun(X)->get_alt_text(X) end, Timeline),
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


-endif.
