-module(ianisqatsibot).
-export([get_alt_texts/0]).

get_alt_texts()->
    {Consumer, AccessToken, AccessSecret}=erlybird:get_secrets(),
    
    Timeline = erlybird:get_user_timeline([{count, "200"},{include_ext_alt_text, "true"}, {screen_name, "igb"}, {include_rts, "false"}], Consumer, AccessToken, AccessSecret),
    AltTexts = lists:map(fun(X)->
			 {Tweet} = X,
			 case lists:keyfind(<<"extended_entities">>, 1, Tweet) of
			     false ->
				 false;
			     {<<"extended_entities">>, {ExtendedEntities}} ->
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
						       
			 end
	      end, Timeline),
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
    
    
