-module(fibonacci).
-export([fib/1, fibSequential/2, fibParallel/2, fibThread/2]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N>1 -> fib(N-1) + fib(N-2);
fib(_) -> undefined.

%% Three fib numbers in serial, as three values in a list.
fibSequential(NumCalcs, FibNum) ->
  [fib(FibNum), fib(FibNum), fib(FibNum)].






%% A process to calculate a Fibonacci number and send it back
%% to a waiting process process.
fibThread(SendTo, N) ->
  Answer = fib(N),
  SendTo ! {self(), Answer}.

%% A process to calculate 3 fib numbers by spawning 3 threads, sending
%% each a message, then handling their responses to build a list of
%% 3 values.
fibParallel(NumThreads, FibNum) ->
  _ = [spawn(?MODULE, fibThread, [self(), FibNum]) || _ <- lists:seq(1, NumThreads)],
  %% Await the replies; 3 expected. Return them.
  awaitfibs(NumThreads, []).

%% A loop to receive a certain number of messages contining fib
%% results, then return a list containing all the results.
awaitfibs(ThreadsLeft, Results) ->
  receive
    {_, FibN} when ThreadsLeft =:= 1 ->
      [FibN|Results];
    {_, FibN} ->
      awaitfibs(ThreadsLeft - 1, [FibN|Results])
  end.