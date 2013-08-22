%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    sysctl api
%%% @end
%%% Created : 14 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-module(sysctl).

-export([get/1]).
-export([nametomib/1]).
-export([get_mib/1, set_mib/2]).

-on_load(init/0).

init() ->
    Nif = filename:join(code:priv_dir(sysctl), "sysctl_nif"),
    erlang:load_nif(Nif, 0).

get_mib(_Mib) ->
    erlang:error(nif_not_loaded).

set_mib(_Mib, _Value) ->
    erlang:error(nif_not_loaded).

get(Name) ->
    case sysctl_util:nametomib(Name) of
	{Mib,Type} ->
	    decode(Type, get_mib(Mib));
	error ->
	    error
    end.

nametomib(Name) ->
    sysctl_util:nametomib(Name).

decode(node, Bin) -> Bin;
decode(int32,  <<Int:32/little>>) -> Int;
decode(string, Bin) -> binary:bin_to_list(Bin, 0, byte_size(Bin)-1);
decode(int64, <<Int:64/little>>) -> Int;
decode(binary, Bin) -> Bin;
decode(struct, Bin) -> Bin;
decode({struct,_Struct},Bin) -> Bin.
