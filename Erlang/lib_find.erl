-module(lib_find).
-export([files/3, files/5]).
-import(lists, [reverse/1]).

-include_lib("kernel/include/file.hrl").

files(Dir, Wildcard, Flag) ->
  reverse(files(Dir, Wildcard, Flag, fun(File, Acc) -> [File|Acc] end, [])).
  
files(Dir, Wildcard, Recursive, Fun, Acc) ->
  case file:list_dir(Dir) of
    {ok, Files} -> find_files(Files, Dir, Wildcard, Recursive, Fun, Acc);
    {error, _} -> Acc
  end.
  
find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
  FullName = filename:join(Dir, File),
  case file_type(FullName) of
    regular ->
      case regexp:match(FullName, Reg) of
        {match, _, _} ->
          Acc = Fun(FullName, Acc0),
          find_files(T, Dir, Reg, Recursive, Fun, Acc);
        _ ->
          find_files(T, Dir, Reg, Recursive, Fun, Acc0)
      end;
    directory ->
      case Recursive of
        true ->
          Acc1 = files(FullName, Reg, Recursive, Fun, Acc0),
          find_files(T, Dir, Reg, Recursive, Fun, Acc1);
        false ->
          find_files(T, Dir, Reg, Recursive, Fun, Acc0)
      end;
    error ->
      find_files(T, Dir, Reg, Recursive, Fun, Acc0)
  end;
find_files([], _, _, _, _, A) ->
  A.
  
file_type(File) ->
  case file:read_file_info(File) of
    {ok, Facts} ->
      case Facts#file_info.type of
        regular -> regular;
        directory -> directory;
        _ -> error
      end;
    _ ->
      error
  end.