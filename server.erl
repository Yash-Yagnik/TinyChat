% Yash Yagnik

-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
		undefined -> ok;
		TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
		#serv_st{
			nicks = maps:new(), %% nickname map. client_pid => "nickname"
			registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 		chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
		}
    ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState = 
			#serv_st{
				nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
				registrations = State#serv_st.registrations,
				chatrooms = State#serv_st.chatrooms
			},
			loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% Handles the join protocol from the server's perspective
do_join(ChatName, ClientPID, Ref, State) ->
    case maps:find(ChatName, State#serv_st.chatrooms) of
        {ok, RPID} ->
            Updated_users = maps:put(ChatName, lists:append(maps:get(ChatName, State#serv_st.registrations), [ClientPID]), State#serv_st.registrations),
            RPID!{self(), Ref, register, ClientPID, maps:get(ClientPID, State#serv_st.nicks)},
            State#serv_st{registrations = Updated_users};
		error ->
            RPID = spawn(chatroom, start_chatroom, [ChatName]),
            New_state = State#serv_st{chatrooms = maps:put(ChatName, RPID, State#serv_st.chatrooms), registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations)},
			Updated_users = maps:put(ChatName, lists:append(maps:get(ChatName, New_state#serv_st.registrations), [ClientPID]), New_state#serv_st.registrations),
            RPID!{self(), Ref, register, ClientPID, maps:get(ClientPID, New_state#serv_st.nicks)},
            New_state#serv_st{registrations = Updated_users}
    end.

%% Handles the leave protocol from the server's perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    RPID = maps:get(ChatName, State#serv_st.chatrooms),
    Update_members = lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)),
    RPID!{self(), Ref, unregister, ClientPID},
    ClientPID!{self(), Ref, ack_leave},
    State#serv_st{registrations = maps:put(ChatName, Update_members, State#serv_st.registrations)}.

%% Handles nickname changes from the server's perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    ExistingNicks = maps:values(State#serv_st.nicks),
    case lists:member(NewNick, ExistingNicks) of
        true ->
            ClientPID!{self(), Ref, err_nick_used},
            State;
        false ->
            Update = State#serv_st{nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)},
            lists:foreach(fun(ChatName) ->
				Users_in_chatroom = maps:get(ChatName, Update#serv_st.registrations),
				lists:foreach(fun(UserPID) ->
					case ClientPID == UserPID of 
						true ->
							RPID = maps:get(ChatName, Update#serv_st.chatrooms),
							RPID!{self(), Ref, update_nick, ClientPID, NewNick};
						false -> 
							ok
					end
				end, Users_in_chatroom)
            end, maps:keys(Update#serv_st.registrations)),
            ClientPID!{self(), Ref, ok_nick},
            Update
    end.

%% Handles client quit protocol from the server's perspective
do_client_quit(State, Ref, ClientPID) ->
    lists:foreach(fun(ChatName) ->
        Users_in_chatroom = maps:get(ChatName, State#serv_st.registrations),
        lists:foreach(fun(MemberPID) ->
            case ClientPID == MemberPID of
                true ->
                    RPID = maps:get(ChatName, State#serv_st.chatrooms),
                    RPID!{self(), Ref, unregister, ClientPID};
                false ->
                    ok
            end
        end, Users_in_chatroom)
    end, maps:keys(State#serv_st.registrations)),
    Updated_names = maps:remove(ClientPID, State#serv_st.nicks),
	Updated_registrations = State#serv_st.registrations,
	lists:foreach(fun(ChatName) ->
		RoomMembers = maps:get(ChatName, Updated_registrations),
		UpdatedRoomMembers = lists:delete(ClientPID, RoomMembers),
		put(ChatName, UpdatedRoomMembers)
	end, State#serv_st.registrations),
    ClientPID!{self(), Ref, ack_quit},
    State#serv_st{nicks = Updated_names, registrations = Updated_registrations}.
