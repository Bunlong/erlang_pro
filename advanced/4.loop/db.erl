%%%-------------------------------------------------------------------
%%% @author Bunlong <bunlong.van@gmail.com>
%%% @copyright (C) 2015, Bunlong
%%% @doc
%%%
%%% @end
%%% Created : 04 Jan 2015 by Bunlong <bunlong.van@gmail.com>
%%%-------------------------------------------------------------------
-module(db).
-include("db.hrl").
-include("/usr/lib/erlang/lib/stdlib-1.19.4/include/qlc.hrl").

%% API
-export([create_table/0, add_record/1, get_user_is_not_admin/0, get_user_is_admin/0, get_username_jobtitle/0, get_jobtitle_translationoriginal/0, get_jobtitle_translationoriginal2/0, get_all_user/0, get_user_by_name/1, get_username_jobtitle_by_username/1, get_jobtitle_translationoriginal_by_jobtitle/1, update_jobtitle_by_jobid/2]).

%% db:create_table().
create_table() ->
    mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()|nodes()]}]),
    mnesia:create_table(job, [{attributes, record_info(fields, job)}, {disc_copies, [node()|nodes()]}]),
    mnesia:create_table(translation, [{attributes, record_info(fields, translation)}, {disc_copies, [node()|nodes()]}]).

%% db:add_record(#user{name="Bunlong", password="123", job=[1]}).
add_record(Rec) ->
    Fun = fun() -> mnesia:write(Rec) end,
    mnesia:transaction(Fun).

%% db:get_user_is_not_admin().
get_user_is_not_admin() ->
    Fun = fun() ->
      qlc:e(qlc:q([User || User <- mnesia:table(user), User#user.admin =/= true]))
    end,
    mnesia:transaction(Fun).

%% db:get_user_is_admin().
get_user_is_admin() ->
    Fun = fun() ->
      qlc:e(qlc:q([User || User <- mnesia:table(user), User#user.admin == true]))
    end,
    mnesia:transaction(Fun).

%% db:get_username_jobtitle().
get_username_jobtitle() ->
    Fun = fun() ->
      qlc:e(qlc:q([{Name, Title} || #user{name=Name, job=JobList} <- mnesia:table(user), #job{id=Id, title=Title} <- mnesia:table(job), lists:member(Id, JobList)]))
    end,
    mnesia:transaction(Fun).

%% db:get_jobtitle_translationoriginal().
get_jobtitle_translationoriginal() ->
    Fun = fun() ->
      qlc:e(qlc:q([{J#job.title, T#translation.original} || J <- mnesia:table(job), T <- mnesia:table(translation), J#job.id == T#translation.job_id]))
    end,
    mnesia:transaction(Fun).

%% db:get_jobtitle_translationoriginal2().
get_jobtitle_translationoriginal2() ->
    Fun = fun() ->
      qlc:e(qlc:q([{Title, Original} || #job{title=Title, id=Id} <- mnesia:table(job), #translation{original=Original, job_id=Jobid} <- mnesia:table(translation), Id==Jobid]))
    end,
    mnesia:transaction(Fun).

%% db:get_all_user().
get_all_user() ->
    Fun = fun() ->
      qlc:e(qlc:q([User || User <- mnesia:table(user)]))
    end,
    mnesia:transaction(Fun).

%% db:get_user_by_name(Name)
get_user_by_name(Name) ->
    Fun = fun() ->
      qlc:e(qlc:q([User || User <- mnesia:table(user), User#user.name==Name]))
    end,
    mnesia:transaction(Fun).

%% db:get_username_jobtitle_by_username("Bunlong")
get_username_jobtitle_by_username(User_Name) ->
    Fun = fun() ->
      qlc:e(qlc:q([{Name, Title} || #user{name=Name, job=Job_List} <- mnesia:table(user), #job{id=Id, title=Title} <- mnesia:table(job), lists:member(Id, Job_List), Name == User_Name]))
    end,
    mnesia:transaction(Fun).

%% andalso, orelse
%% db:get_jobtitle_translationoriginal_by_jobtitle("Software Engineer").
get_jobtitle_translationoriginal_by_jobtitle(Job_Title) ->
    Fun = fun() ->
      qlc:e(qlc:q([{Title, Original} || #job{id=Id, title=Title} <- mnesia:table(job), #translation{original=Original, job_id=Job_Id} <- mnesia:table(translation), Id==Job_Id andalso Title==Job_Title]))
    end,
    mnesia:transaction(Fun).

%% db:update_jobtitle_by_jobid("Software Developer", 1).
update_jobtitle_by_jobid(Job_Title, Job_Id) ->
    Fun = fun() ->
      Job_R = qlc:e(qlc:q([Job || Job <- mnesia:table(job), Job#job.id == Job_Id])),
      mnesia:write(Job_R#job{title=Job_Title})
    end,
    mnesia:transaction(Fun).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
