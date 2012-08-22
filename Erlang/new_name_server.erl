-module(new_name_server).
-export([init/0, add/2, all_names/0, delete/1, whereis/1, handle/2]).
-import(server3, [rpc/2]).

all_names() -> rpc(name_server, {allNames}).
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
whereis(Name) -> rpc(name_server, {whereis, Name}).
delete(Name) -> rpc(name_server, {delete, Name}).

init() -> dict:new().

handle({allNames}, Dict) -> {dict:fetch_keys(Dict), Dict};
handle({add, Name, Place}, Dict) -> {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) -> {dict:find(Name, Dict), Dict};
handle({delete, Name}, Dict) -> {ok, dict:erase(Name, Dict)}.