%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for mochirest.

-module(mochirest_web).
-author('author <author@example.com>').

-import(random).
-export([start/1, stop/0, loop/2]).

-record(doc, {id, name, body}).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    io:format("~p ~p~n", [Req:get(path), Req:get(method)]),
    case {Req:get(path), Req:get(method)} of
        {"/documents.json", 'GET'} ->
            all(Req, DocRoot);
        {"/documents.json", 'POST'} ->
            create(Req, DocRoot);
        {_, 'GET'} ->
            read(Req, DocRoot);
        {_, 'PUT'} ->
            update(Req, DocRoot);
        {_, 'DELETE'} ->
            delete(Req, DocRoot);
        {_, _} ->
            Req:respond({501, [], []})
    end.

%% Internal API

all(Req, DocRoot) ->
    % All = lists:map(fun(FN) ->
    %                         ID=id_from_file(FN),
    %                         Doc=load(ID, DocRoot),
    %                         {struct, tuples(Doc)}
    %                 end, FList),
    All = get_all(),
    Req:respond({200, [], rfc4627:encode(All) }).

create(Req, DocRoot) ->
    Body = Req:recv_body(),
    io:format("Create: ~p~n", [Body]),
    {ok, Doc,_} = rfc4627:decode(Body),
    io:format("... Doc: ~p~n", [Doc]),
    {ok, {obj, Result}} = post_doc(Doc),
    [DocId | _] = [ X || {"id", X} <- Result],
    NewDoc = get_doc(DocId),
    Req:respond({300,
                 [{status, created},
                  {location, "/documents/" ++ DocId}],
                 [rfc4627:encode(NewDoc)]}).
                 
doc_id(Req) ->
    "/documents/" ++ Postfix = Req:get(path),
    "nosj." ++ DocId = lists:reverse(Postfix),
    lists:reverse(DocId).

read(Req, DocRoot) ->
    DocId = doc_id(Req),
    io:format("DocId: ~p~n", [DocId]),
    Doc = get_doc(DocId),
    Req:respond({200, [], rfc4627:encode(Doc) }).

update(Req, DocRoot) ->
    Body = Req:recv_body(),
    io:format("Update: ~p~n", [Body]),
    {ok, Doc,_} = rfc4627:decode(Body),
    io:format("... Doc: ~p~n", [Doc]),
    
    put_doc(Doc),
    Req:respond({200, [], rfc4627:encode(Doc)}).

delete(Req, DocRoot) ->
    DocId = doc_id(Req),
    delete_doc(DocId),
    Req:respond({200, [], []}).

from_json(ID, JSON) ->
    {obj, Props} = rfc4627:decode(JSON),
    #doc{id=ID,
         name=proplists:get_value("name", Props),
         body=proplists:get_value("body", Props)}.

load(ID, DocRoot) ->
    FN = filename:join(DocRoot, file_from_id(ID)),
    {ok, Bin} = file:read_file(FN),
    from_json(ID, binary_to_list(Bin)).

all_to_json(DocRoot) ->
    {ok, FList} = file:list_dir(DocRoot),
    All = lists:map(fun(FN) ->
                            {struct, tuples(load(id_from_file(FN), DocRoot))}
                    end,
                    FList),
    mochijson:encode({array, All}).

tuples(Doc) ->
    [{id, Doc#doc.id}, {name, Doc#doc.name}, {body, Doc#doc.body}].

to_json(Doc) ->
    mochijson:encode({struct, tuples(Doc)}).

save(Doc, DocRoot) ->
    FileName = filename:join([DocRoot, file_from_id(Doc#doc.id)]),
    {ok, WriteIoDevice} = file:open(FileName, write),
    ok = file:write(WriteIoDevice, to_json(Doc)),
    ok = file:close(WriteIoDevice).

file_from_req(Req) ->
    "/documents/" ++ FileName = Req:get(path),
    FileName.

id_from_file(Path) ->
    "nosj." ++ RID = lists:reverse(Path),
    lists:reverse(RID).

file_from_id(ID) ->
    ID ++ ".json".

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_all() ->
    % All = ecouch:doc_get_all("documents"),
    All = ecouch:doc_get("documents","_view/documents/all-map"),
    io:format("Raw Results: ~p~n",[All]),
    clean_view_results(All).

get_doc(DocId) ->
    {ok, Doc} = ecouch:doc_get("documents",DocId),
    io:format("Raw Doc: ~p~n",[Doc]),
    Doc.    

rows(Results) ->    
    {_,{_,[_,_,{_,Rows}]}} = Results,
    io:format("Pattern Matched Rows: ~p~n",[Rows]),
    Rows.

result(Row) ->
    {_,Result} = Row,
    io:format("Pattern Matched Result: ~p~n",[Result]),
    Result.

value(Result) ->
    [Value | _] = [X || {"value", X} <- Result],
    io:format("Pattern Matched Value: ~p~n",[Value]),
    Value.

value_from_row(Row) -> 
    value(result(Row)).
    
clean_view_results(Results) ->
    Rows = rows(Results),
    % Values = [ value(result(X)) || X < Result ]
    % Values.
    Values = lists:map(fun(Row) -> value_from_row(Row) end, Rows),
    % [Row | T] = Rows,
    % Value = value_from_row(Row),
    % Value.
    io:format("Pattern Matched Values: ~p~n",[Values]),
    Values.
    
post_doc(Doc) ->
    io:format("POST to Couch: ~p~n", [Doc]),
    Result = ecouch:doc_create("documents",Doc),
    io:format(" ... result: ~p~n", [Result]),
    Result.

delete_doc(DocId) ->
    io:format("DELETE to Couch: ~p~n", [DocId]),
    {ok, {obj, Doc}} = ecouch:doc_get("documents",DocId),
    io:format("... Doc from Couch: ~p~n", [Doc]),
    [Rev | _ ] = [X || {"_rev",X} <- Doc],
    io:format("... _rev: ~p~n", [Rev]),
    Result = ecouch:doc_delete("documents",DocId,Rev),
    io:format(" ... result: ~p~n", [Result]),
    Result.
    
put_doc(Doc) ->
    io:format("PUT ~p~n", [Doc]),
    {obj,DocYargh} = Doc,
    [DocId | _] = [ X || {"_id", X} <- DocYargh],
    io:format("PUT to Couch -- Doc: ~p~n -- DocId: ~p~n", [Doc,DocId]),
    Result = ecouch:doc_update("documents", DocId, Doc),
    io:format(" ... result: ~p~n", [Result]),
    Result.
    