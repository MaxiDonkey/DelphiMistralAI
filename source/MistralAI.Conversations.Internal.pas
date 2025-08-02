unit MistralAI.Conversations.Internal;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types,
  MistralAI.Conversations.Params, MistralAI.Conversations.Chunks, MistralAI.Async.Support,
  MistralAI.Async.Params, MistralAI.Async.Promise, MistralAI.Conversations.Manager,
  MistralAI.Conversations.EventStreaming, MistralAI.API.Normalizer;

type
  /// <summary>
  /// Defines a reference callback type for handling streamed conversation event chunks.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TConversationsEventRef</c> is a procedure reference that is invoked for each chunk of a streaming
  /// conversation response. It provides the <see cref="TConversationsEvent"/> chunk, a flag indicating
  /// whether the stream is complete (<c>IsDone</c>), and a <c>Cancel</c> parameter that can be set to
  /// <c>True</c> to abort the stream.
  /// </para>
  /// <para>
  /// Implement this callback to process incoming event data incrementally and to control stream cancellation.
  /// </para>
  /// </remarks>
  TConversationsEventRef = reference to procedure(var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TConversation"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversation</c> is an alias for <c>TAsyncCallback&lt;TConversation&gt;</c>, providing a consistent pattern
  /// for handling the lifecycle of asynchronous conversation calls.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers
  /// when invoking methods that produce a <see cref="TConversation"/> asynchronously.
  /// </para>
  /// </remarks>
  TAsyncConversation = TAsyncCallback<TConversation>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TConversation"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversation</c> is an alias for <c>TPromiseCallback&lt;TConversation&gt;</c>, offering a task-like interface
  /// for awaiting the result of an asynchronous conversation operation.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the completion of a conversation request—resolving with a <see cref="TConversation"/>
  /// on success or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseConversation = TPromiseCallback<TConversation>;

  /// <summary>
  /// Defines an asynchronous streaming callback type for conversation events.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversationsEvent</c> is an alias for <c>TAsyncStreamCallBack&lt;TConversationsEvent&gt;</c>,
  /// providing a mechanism to handle streamed chunks of <see cref="TConversationsEvent"/> as they arrive.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers
  /// when invoking streaming conversation APIs.  Progress events deliver each <see cref="TConversationsEvent"/> chunk,
  /// and the final success event signals completion of the stream.
  /// </para>
  /// </remarks>
  TAsyncConversationsEvent = TAsyncStreamCallBack<TConversationsEvent>;

  /// <summary>
  /// Defines a promise-based asynchronous streaming callback for conversation events.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversationsEvent</c> is an alias for <c>TPromiseStreamCallBack&lt;TConversationsEvent&gt;</c>,
  /// providing a task-like interface for handling streamed conversation event chunks.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the full event stream—receiving each <see cref="TConversationsEvent"/> incrementally
  /// via progress callbacks and resolving when the stream completes, or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseConversationsEvent = TPromiseStreamCallBack<TConversationsEvent>;

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TConversationsList"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversationsList</c> is an alias for <c>TAsyncCallback&lt;TConversationsList&gt;</c>, providing a structured mechanism
  /// for handling the lifecycle of asynchronous calls that produce a <see cref="TConversationsList"/>.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers when invoking methods
  /// that retrieve or process a list of conversations asynchronously.
  /// </para>
  /// </remarks>
  TAsyncConversationsList = TAsyncCallback<TConversationsList>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TConversationsList"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversationsList</c> is an alias for <c>TPromiseCallback&lt;TConversationsList&gt;</c>, offering a task-like interface
  /// for awaiting the result of an asynchronous conversation-list operation.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the completion of a request that retrieves a <see cref="TConversationsList"/>;
  /// it resolves with the list on success or raises an exception on error.
  /// </para>
  /// </remarks>
  TPromiseConversationsList = TPromiseCallback<TConversationsList>;

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TConversationsListItem"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversationsListItem</c> is an alias for <c>TAsyncCallback&lt;TConversationsListItem&gt;</c>, providing a consistent pattern
  /// for handling the lifecycle of asynchronous calls that yield a single conversation list item.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers when invoking methods
  /// that fetch or process a <see cref="TConversationsListItem"/> asynchronously.
  /// </para>
  /// </remarks>
  TAsyncConversationsListItem = TAsyncCallback<TConversationsListItem>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TConversationsListItem"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversationsListItem</c> is an alias for <c>TPromiseCallback&lt;TConversationsListItem&gt;</c>, providing
  /// a task-like interface for awaiting the outcome of an asynchronous operation that yields a single conversation list item.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the retrieval or processing of a <see cref="TConversationsListItem"/>, resolving
  /// with the item on success or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseConversationsListItem = TPromiseCallback<TConversationsListItem>;

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TRetrievedEntries"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncRetrievedEntries</c> is an alias for <c>TAsyncCallback&lt;TRetrievedEntries&gt;</c>, providing
  /// a structured mechanism for handling the lifecycle of asynchronous calls that produce a <see cref="TRetrievedEntries"/>.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers when invoking methods
  /// that retrieve or process entries asynchronously.
  /// </para>
  /// </remarks>
  TAsyncRetrievedEntries = TAsyncCallback<TRetrievedEntries>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TRetrievedEntries"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseRetrievedEntries</c> is an alias for <c>TPromiseCallback&lt;TRetrievedEntries&gt;</c>, offering a task-like interface
  /// for awaiting the result of an asynchronous entries retrieval operation.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the completion of a request that fetches <see cref="TRetrievedEntries"/>,
  /// resolving with the entries on success or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseRetrievedEntries = TPromiseCallback<TRetrievedEntries>;

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TRetrieveMessages"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncRetrieveMessages</c> is an alias for <c>TAsyncCallback&lt;TRetrieveMessages&gt;</c>, providing
  /// a consistent pattern for handling the lifecycle of asynchronous calls that retrieve messages.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers when invoking
  /// methods that fetch message data asynchronously.
  /// </para>
  /// </remarks>
  TAsyncRetrieveMessages = TAsyncCallback<TRetrieveMessages>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TRetrieveMessages"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseRetrieveMessages</c> is an alias for <c>TPromiseCallback&lt;TRetrieveMessages&gt;</c>, offering
  /// a task-like interface for awaiting the result of an asynchronous message retrieval operation.
  /// </para>
  /// <para>
  /// Use this promise to <c>await</c> the completion of a request that fetches <see cref="TRetrieveMessages"/>,
  /// resolving with the retrieved messages on success or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseRetrieveMessages = TPromiseCallback<TRetrieveMessages>;

  TConversationsRouteInternal = class(TMistralAIAPIRoute)
  protected
    /// <summary>
    /// Performs a low‑level streaming request to the specified conversations endpoint.
    /// </summary>
    /// <param name="Endpoint">
    /// The relative API endpoint (for example, 'conversations' or 'conversations/{id}/stream')
    /// to which the streaming POST will be sent.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model,
    /// prompt, and any additional streaming options.
    /// </param>
    /// <param name="Event">
    /// A callback reference (<see cref="TConversationsEventRef"/>) invoked for each incoming
    /// <see cref="TConversationsEvent"/> chunk. The <c>IsDone</c> flag indicates end‑of‑stream,
    /// and setting <c>Cancel</c> to <c>True</c> aborts the stream.
    /// </param>
    /// <returns>
    /// <c>True</c> if the stream completed successfully or was cancelled via the callback;
    /// <c>False</c> if an unrecoverable error occurred during the HTTP transfer.
    /// </returns>
    /// <remarks>
    /// <para>
    /// This method issues a blocking HTTP POST to the given <paramref name="Endpoint"/>,
    /// writing raw response data into a buffer and parsing individual SSE‑style lines.
    /// For each non‑empty line, it normalizes and deserializes the JSON payload into
    /// a <see cref="TConversationsEvent"/> object, then invokes the <paramref name="Event"/>
    /// callback on the main thread to allow incremental UI updates or cancellation checks.
    /// </para>
    /// <para>
    /// When the special “[DONE]” sentinel is encountered, <c>IsDone</c> is passed as
    /// <c>True</c> and the loop breaks, triggering the final completion behavior.
    /// Any deserialization errors for individual chunks are swallowed, allowing
    /// the stream to continue until cancelled or finished.
    /// </para>
    /// </remarks>
    function CreateStreamInternal(const Endpoint: string;
      const ParamProc: TProc<TConversationsParams>;
      const Event: TConversationsEventRef): Boolean;

    /// <summary>
    /// Initiates an asynchronous streaming request to a conversations endpoint using callbacks.
    /// </summary>
    /// <param name="Endpoint">
    /// The relative API endpoint (e.g., 'conversations/stream') to which the streaming POST will be sent.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsParams"/>, including model settings, prompt text,
    /// and streaming options such as max tokens or stop sequences.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <see cref="TAsyncConversationsEvent"/> instance, where you can assign
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, <c>OnError</c>, and <c>OnDoCancel</c> handlers
    /// to manage the streaming lifecycle.
    /// </param>
    /// <remarks>
    /// <para>
    /// This method creates a background task that performs a non-blocking HTTP POST to the specified
    /// <paramref name="Endpoint"/>. It parses server-sent events (SSE) line by line, deserializes each
    /// JSON chunk into a <see cref="TConversationsEvent"/>, and invokes the <c>OnProgress</c> callback
    /// for each chunk. When the stream completes (or the callback signals cancellation), the
    /// <c>OnSuccess</c> or <c>OnCancellation</c> handler is called on the main thread.
    /// </para>
    /// <para>
    /// Errors during streaming trigger the <c>OnError</c> handler with the exception message.
    /// Cancellation is cooperative: if <c>OnDoCancel</c> returns <c>True</c>, the HTTP read loop aborts.
    /// </para>
    /// </remarks>
    procedure AsyncStreamInternal(
      const Endpoint  : string;
      const ParamProc : TProc<TConversationsParams>;
      const Callbacks : TFunc<TAsyncConversationsEvent>);

    /// <summary>
    /// Initiates an asynchronous streaming request to a conversations endpoint and returns a promise for the full response.
    /// </summary>
    /// <param name="Endpoint">
    /// The relative API endpoint (for example, 'conversations/{id}/stream') to which the streaming POST will be sent.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsParams"/>, such as model selection, prompt text,
    /// and any streaming options (e.g., max tokens, stop sequences).
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <see cref="TPromiseConversationsEvent"/> instance, where you can assign
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnError</c>, and <c>OnDoCancel</c> handlers. Progress callbacks
    /// receive each <see cref="TConversationsEvent"/> chunk, and cancellation can be requested via <c>OnDoCancel</c>.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{String}"/> that resolves with the concatenated text of all chunks when the stream completes,
    /// or rejects with an exception if an error occurs or the stream is aborted.
    /// </returns>
    /// <remarks>
    /// <para>
    /// This method wraps the callback-based <see cref="AsyncStreamInternal"/> into a promise-based interface.
    /// It starts a background task that parses server-sent events (SSE) line by line, deserializes JSON
    /// into <see cref="TConversationsEvent"/> objects, and buffers text content. Each chunk triggers the
    /// <c>OnProgress</c> handler. When the special "[DONE]" sentinel is encountered, the accumulated buffer
    /// is resolved via the promise. Errors trigger promise rejection, and setting <c>Cancel</c> in <c>OnDoCancel</c>
    /// aborts the stream and rejects the promise with an "aborted" exception.
    /// </para>
    /// </remarks>
    function AsyncAwaitStreamInternal(
      const Endpoint  : string;
      const ParamProc : TProc<TConversationsParams>;
      const Callbacks : TFunc<TPromiseConversationsEvent>): TPromise<string>;
  end;

implementation

{ TConversationsRouteInternal }

function TConversationsRouteInternal.AsyncAwaitStreamInternal(
  const Endpoint: string; const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversationsEvent>): TPromise<string>;
begin
  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    var
      Buffer: string;
    begin
      AsyncStreamInternal(Endpoint, ParamProc,
        function : TAsyncConversationsEvent
        begin
          Result.Sender := Callbacks.Sender;

          Result.OnStart := Callbacks.OnStart;

          Result.OnProgress :=
            procedure (Sender: TObject; Event: TConversationsEvent)
            begin
              if Assigned(Callbacks.OnProgress) then
                Callbacks.OnProgress(Sender, Event);

              case Event.&Type of
                TChunkEvent.conversation_response_done:
                  Resolve(Buffer);

                TChunkEvent.conversation_response_error:
                  Reject(Exception.Create(Event.Message));

                else
                  begin
                    for var Item in Event.Content do
                      if Item.&Type = TContentChunkType.text then
                        Buffer := Buffer + Item.Text;
                  end;
              end;
            end;

          Result.OnError :=
            procedure (Sender: TObject; Error: string)
            begin
              if Assigned(Callbacks.OnError) then
                Error := Callbacks.OnError(Sender, Error);
              Reject(Exception.Create(Error));
            end;

          Result.OnDoCancel :=
            function : Boolean
            begin
              if Assigned(Callbacks.OnDoCancel) then
                Result := Callbacks.OnDoCancel()
              else
                Result := False;
            end;

          Result.OnCancellation :=
            procedure (Sender: TObject)
            begin
              var Error := 'aborted';
              if Assigned(Callbacks.OnCancellation) then
                Error := Callbacks.OnCancellation(Sender);
              Reject(Exception.Create(Error));
            end;
        end);
    end);
end;

procedure TConversationsRouteInternal.AsyncStreamInternal(
  const Endpoint: string; const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversationsEvent>);
begin
  var CallbackParams := TUseParamsFactory<TAsyncConversationsEvent>.CreateInstance(Callbacks);

  var Sender := CallbackParams.Param.Sender;
  var OnStart := CallbackParams.Param.OnStart;
  var OnSuccess := CallbackParams.Param.OnSuccess;
  var OnProgress := CallbackParams.Param.OnProgress;
  var OnError := CallbackParams.Param.OnError;
  var OnCancellation := CallbackParams.Param.OnCancellation;
  var OnDoCancel := CallbackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
          procedure()
          begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStreamInternal(Endpoint, ParamProc,
                procedure (var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  if not IsDone and Assigned(Chunk) then
                    begin
                      var LocalChunk := Chunk;
                      Chunk := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChunk);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChunk.Free;
                          end;
                        end)
                     else
                       LocalChunk.Free;
                    end
                  else
                  if IsDone then
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnSuccess(Sender);
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);
  Task.Start;
end;

function TConversationsRouteInternal.CreateStreamInternal(
  const Endpoint: string; const ParamProc: TProc<TConversationsParams>;
  const Event: TConversationsEventRef): Boolean;
var
  Response: TStringStream;
  RetPos, LineFeed: Integer;
  ResponseBuffer, CurrentLine, Data, NewBuffer: string;
  Chunk: TConversationsEvent;
  IsDone: Boolean;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TConversationsParams>(Endpoint, ParamProc, Response,
      procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
      begin
        try
          ResponseBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
        while LineFeed >= 0 do
          begin
            CurrentLine := ResponseBuffer.Substring(RetPos, LineFeed - RetPos).Trim([' ', #13, #10]);
            Inc(RetPos, LineFeed - RetPos + 1);

            if CurrentLine.IsEmpty then
              begin
                LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
                Continue;
              end;

            if CurrentLine.StartsWith('data: ') then
              Data := CurrentLine.Substring(6).Trim
            else
              Data := CurrentLine;

            IsDone := SameText(Data, '[DONE]');

            Chunk := nil;
            if not IsDone then
              try
                Chunk := TApiDeserializer.Parse<TConversationsEvent>(
                  TJSONNormalizer.Normalize(Data, ['content']));
              except
                Chunk := nil;
              end;

            if Assigned(Event) then
              try
                Event(Chunk, IsDone, AAbort);
              finally
                Chunk.Free;
              end;

            if IsDone then
              Break;

            LineFeed := ResponseBuffer.IndexOf(#10, RetPos);
          end;

        if RetPos > 0 then
          begin
            NewBuffer := ResponseBuffer.Substring(RetPos);
            Response.Size := 0;
            if NewBuffer <> '' then
              Response.WriteString(NewBuffer);
            RetPos := 0;
          end;
      end);
  finally
    Response.Free;
  end;
end;

end.
