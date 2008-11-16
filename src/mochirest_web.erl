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
    Doc = from_json(uuid:gen(), Req:recv_body()),
    save(Doc, DocRoot),
    Req:respond({300,
                 [{status, created},
                  {location, "/documents/" ++ file_from_id(Doc#doc.id)}],
                 [to_json(Doc)]}).

read(Req, DocRoot) ->
    Req:serve_file(file_from_req(Req), DocRoot).

update(Req, DocRoot) ->
    Doc = from_json(id_from_file(file_from_req(Req)), Req:recv_body()),
    save(Doc, DocRoot),
    Req:respond({200, [], [to_json(Doc)]}).

delete(Req, DocRoot) ->
    file:delete(filename:join(DocRoot, file_from_req(Req))),
    Req:respond({200, [], []}).

from_json(ID, JSON) ->
    {struct, Props} = mochijson:decode(JSON),
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
    
clean_view_results(Results) ->
    {_,{_,[_,_,{_,Rows}]}} = Results,
    io:format("Pattern Matched Rows: ~p~n",[Rows]),
    Rows.
