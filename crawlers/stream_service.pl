:- module(stream_service, [call_with_stream/2]).


% Call Goal with a file stream as last argument
% Forwards FileName, Mode, Options to open
call_with_file_stream(FileName, Mode, Options, Goal) :-
  setup_call_cleanup(
    open(FileName, Mode, Stream, Options),
    call(Goal, Stream),
    close(Stream)).
    
  

% Calls WriteGoal with a writable stream as last argument
% Then calls ReadGoal with a readable stream consisting of
% contents written by WriteGoal
call_with_stream(WriteGoal, ReadGoal) :-
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      % Write
      open_memory_file(Handle, write, W, [encoding(octet)]),
      call(WriteGoal, W),
      close(W),

      % Read
      open_memory_file(Handle, read, R, [encoding(octet)]),
      call(ReadGoal, R),
      close(R)
    ),

    free_memory_file(Handle)).
