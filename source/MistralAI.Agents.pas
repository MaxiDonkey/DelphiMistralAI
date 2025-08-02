unit MistralAI.Agents;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, MistralAI.API.Params, MistralAI.API, MistralAI.Functions.Core,
  MistralAI.Functions.Tools, MistralAI.Chat, MistralAI.Async.Params,
  MistralAI.Async.Support, MistralAI.Async.Promise, MistralAI.Types,
  MistralAI.API.Normalizer;

type
  /// <summary>
  /// Alias for <c>TChatMessagePayload</c>, representing an agent message payload.
  /// </summary>
  TAgentMessagePayload = TChatMessagePayload;

  /// <summary>
  /// Represents the parameters used to configure an agent request.
  /// </summary>
  /// <remarks>
  /// Use this class to set various options for the agent, such as model configuration, messages, and other settings.
  /// </remarks>
  TAgentParams = class(TJSONParam)
    /// <summary>
    /// Sets the maximum number of tokens to generate in the completion.
    /// The token count of your prompt plus <c>max_tokens</c> cannot exceed the model's context length.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of tokens to generate in the completion.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>MaxTokens</c> parameter set.
    /// </returns>
    function MaxTokens(const Value: Integer): TAgentParams;

    /// <summary>
    /// Enables or disables streaming of partial progress.
    /// </summary>
    /// <param name="Value">
    /// Set to <c>True</c> to enable streaming mode; otherwise, set to <c>False</c>.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>Stream</c> parameter set.
    /// </returns>
    /// <remarks>
    /// Default is <c>False</c>. When streaming is enabled, partial progress is sent as data-only server-side events.
    /// </remarks>
    function Stream(const Value: Boolean = True): TAgentParams;

    /// <summary>
    /// Sets a string token at which to stop text generation.
    /// </summary>
    /// <param name="Value">
    /// A string token at which to stop the text generation.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>Stop</c> parameter set.
    /// </returns>
    function Stop(const Value: string): TAgentParams; overload;

    /// <summary>
    /// Sets an array of string tokens at which to stop text generation.
    /// </summary>
    /// <param name="Value">
    /// An array of string tokens at which to stop the text generation.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>Stop</c> parameter set.
    /// </returns>
    function Stop(const Value: TArray<string>): TAgentParams; overload;

    /// <summary>
    /// Sets the seed for random sampling to produce deterministic results.
    /// </summary>
    /// <param name="Value">
    /// The seed value for random sampling.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>RandomSeed</c> parameter set.
    /// </returns>
    function RandomSeed(const Value: Integer): TAgentParams;

    /// <summary>
    /// Sets the messages to generate completions for, encoded as a list of dictionaries with role and content.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TAgentMessagePayload</c> representing the messages in the conversation.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>Messages</c> parameter set.
    /// </returns>
    function Messages(const Value: TArray<TChatMessagePayload>): TAgentParams;

    /// <summary>
    /// Specifies the format that the model must output.
    /// </summary>
    /// <param name="Value">
    /// Specifies the desired response format type. For example, 'json_object' to enforce JSON output.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>ResponseFormat</c> parameter set.
    /// </returns>
    /// <remarks>
    /// Default value is { "type": "text" } if <c>ResponseFormat</c> is not called.
    /// </remarks>
    function ResponseFormat(const Value: string = 'json_object'): TAgentParams;

    /// <summary>
    /// Sets the list of available tools for the model.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TChatMessageTool</c> representing the available tools for the model.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>Tools</c> parameter set.
    /// </returns>
    function Tools(const Value: TArray<TChatMessageTool>): TAgentParams;

    /// <summary>
    /// Specifies if and how functions are called during the conversation.
    /// </summary>
    /// <param name="Value">
    /// Specifies how tools are used during the conversation. Options are <c>none</c>, <c>auto</c>, or <c>any</c>.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>ToolChoice</c> parameter set.
    /// </returns>
    /// <remarks>
    /// Default is <c>auto</c>. If set to <c>none</c>, the model will not call a function. If set to <c>auto</c>, the model can choose to generate a message or call a function. If set to <c>any</c>, the model is forced to call a function.
    /// </remarks>
    function ToolChoice(const Value: TToolChoice): TAgentParams;  overload;

    /// <summary>
    /// Configures how the model interacts when required is on.
    /// </summary>
    /// <param name="Value">
    /// The <c>TToolChoice</c> setting for function interaction, with a default of "auto".
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function ToolChoice(const FunctionName: string): TChatParams; overload;

    /// <summary>
    /// Presence_penalty determines how much the model penalizes the repetition of words or phrases
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher presence penalty encourages the model to use a wider variety of words and phrases,
    /// making the output more diverse and creative.
    /// </remarks>
    function PresencePenalty(const Value: Double): TAgentParams;

    /// <summary>
    /// Frequency_penalty penalizes the repetition of words based on their frequency in the generated text.
    /// </summary>
    /// <param name="Value">
    /// number (Presence Penalty) [ -2 .. 2 ]; Default: 0
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A higher frequency penalty discourages the model from repeating words that have already appeared
    /// frequently in the output, promoting diversity and reducing repetition.
    /// </remarks>
    function FrequencyPenalty(const Value: Double): TAgentParams;

    /// <summary>
    /// Enable users to specify expected results, optimizing response times by leveraging known or
    /// predictable content.
    /// </summary>
    /// <param name="Value">The string prediction content.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This approach is especially effective for updating text documents or code files with minimal
    /// changes, reducing latency while maintaining high-quality results.
    /// </remarks>
    function Prediction(const Value: string): TAgentParams;

    /// <summary>
    /// Whether to allow the model to run tool calls in parallel.
    /// </summary>
    /// <param name="Value">If true then enable parallel mode.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Default: true
    /// </remarks>
    function ParallelToolCalls(const Value: Boolean): TAgentParams;

    /// <summary>
    /// Allows toggling between the reasoning mode and no system prompt.
    /// When set to reasoning the system prompt for reasoning models will be used.
    /// </summary>
    function PromptMode(const Value: string = 'reasoning'): TAgentParams;

    /// <summary>
    /// Number of completions to return for each request, input tokens are only billed once.
    /// </summary>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function N(const Value: Integer): TAgentParams;

    /// <summary>
    /// Sets the ID of the agent to use for this completion.
    /// </summary>
    /// <param name="Value">
    /// The identifier of the agent to be used for the completion.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>AgentId</c> parameter set.
    /// </returns>
    function AgentId(const Value: string): TAgentParams;
  end;

  /// <summary>
  /// Provides methods to interact with agents, including creating completions and handling asynchronous operations.
  /// </summary>
  /// <remarks>
  /// Use this class to manage agent interactions, including synchronous and asynchronous requests.
  /// </remarks>
  TAgentRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Initiates an asynchronous agent completion request using an awaitable promise.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the agent request, including model, prompt, tokens, and tool usage.
    /// </param>
    /// <param name="Callbacks">
    /// (Optional) A function returning a <c>TPromiseChat</c> structure containing callback handlers such as <c>OnSuccess</c>, <c>OnError</c>, and <c>OnStart</c>.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TChat&gt;</c> object that can be awaited to retrieve the resulting chat completion.
    /// </returns>
    /// <remarks>
    /// This method wraps the asynchronous execution of an agent call in a promise, allowing a fluent await-style programming model.
    /// It is particularly useful in scenarios where chaining or async/await-like behavior is preferred.
    /// </remarks>
    function AsyncAwaitCreate(const ParamProc: TProc<TAgentParams>;
      const Callbacks: TFunc<TPromiseChat> = nil): TPromise<TChat>;

    /// <summary>
    /// Initiates an asynchronous agent request in streaming mode using an awaitable promise.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the agent request, including model settings, messages, and tool configurations.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TPromiseChatStream</c> record that defines handlers for progress, success, error, cancellation, and optional sender context.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;string&gt;</c> that resolves with the concatenated streamed content when the operation completes successfully.
    /// </returns>
    /// <remarks>
    /// This method wraps an asynchronous streaming request in a promise interface, allowing developers to consume token-by-token progress and still benefit from a final resolved result.
    /// The collected streamed output is automatically buffered and returned as a single string.
    /// </remarks>
    function AsyncAwaitCreateStream(const ParamProc: TProc<TAgentParams>;
      const Callbacks: TFunc<TPromiseChatStream>): TPromise<string>;

    /// <summary>
    /// Creates a completion for an agent synchronously.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the agent request.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgent</c> containing the completion result.
    /// </returns>
    /// <exception cref="MistralAIExceptionAPI"> MistralAIExceptionAPI </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError"> MistralAIExceptionInvalidRequestError </exception>
    /// <remarks>
    /// This function sends a synchronous request to create an agent completion based on the provided parameters.
    ///
    /// <code>
    ///   var Agent := MistralAI.Agent.Create(
    ///     procedure (Params: TAgentParams)
    ///     begin
    ///       // Define agent parameters
    ///     end);
    ///   try
    ///     for var Choice in Agent.Choices do
    ///       WriteLn(Choice.Message.Content);
    ///   finally
    ///     Agent.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TAgentParams>): TChat;

    /// <summary>
    /// Creates a completion for an agent with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the agent request.
    /// </param>
    /// <param name="Event">
    /// An event handler to process each streamed response.
    /// </param>
    /// <returns>
    /// <c>True</c> if the streaming was successful; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    /// The <c>Agent</c> object will be <c>nil</c> when all data is received.
    ///
    /// <code>
    ///    MistralAI.Agent.Create(
    ///     procedure (Params: TAgentParams)
    ///     begin
    ///       // Define agent parameters
    ///     end,
    ///
    ///     procedure(var Agent: TAgent; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       // handle displaying
    ///     end);
    /// </code>
    /// </remarks>
    function CreateStream(ParamProc: TProc<TAgentParams>; Event: TChatEvent): Boolean;

    /// <summary>
    /// Initiates an asynchronous request to create an agent completion based on the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the agent request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous agent operation, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided callbacks.
    /// <code>
    /// // Example usage:
    /// MistralAI.Agent.AsyncCreate(
    ///   procedure(Params: TAgentParams)
    ///   begin
    ///     // Define agent parameters
    ///   end,
    ///   function: TAsynAgent
    ///   begin
    ///     Result.OnSuccess :=
    ///        procedure(Sender: TObject; Agent: TAgent)
    ///        begin
    ///         // Handle success
    ///        end;
    ///     Result.OnError :=
    ///        procedure(Sender: TObject; const ErrorMsg: string)
    ///        begin
    ///          // Handle error
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreate(ParamProc: TProc<TAgentParams>; CallBacks: TFunc<TAsynChat>);

    /// <summary>
    /// Creates an asynchronous streaming agent completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the agent request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynAgentStream</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// // Example usage:
    /// MistralAI.Agent.AsyncStreamCreate(
    ///   procedure(Params: TAgentParams)
    ///   begin
    ///     // Define agent parameters
    ///   end,
    ///   function: TAsynAgentStream
    ///   begin
    ///     Result.OnProgress :=
    ///        procedure (Sender: TObject; Chat : TChat)
    ///        begin
    ///          // Handle progress
    ///        end;
    ///     Result.OnSuccess :=
    ///        procedure(Sender: TObject)
    ///        begin
    ///         // Handle success
    ///        end;
    ///     Result.OnError :=
    ///        procedure(Sender: TObject; const ErrorMsg: string)
    ///        begin
    ///          // Handle error
    ///        end;
    ///     Result.OnDoCancel :=
    ///        function : Boolean
    ///        begin
    ///          Result := CheckBox1.Checked; // Click the CheckBox to cancel
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreateStream(ParamProc: TProc<TAgentParams>; CallBacks: TFunc<TAsynChatStream>);
  end;

implementation

uses
  system.StrUtils, Rest.Json;

{ TAgentParams }

function TAgentParams.AgentId(const Value: string): TAgentParams;
begin
  Result := TAgentParams(Add('agent_id', Value));
end;

function TAgentParams.FrequencyPenalty(const Value: Double): TAgentParams;
begin
  Result := TAgentParams(Add('frequency_penalty', Value));
end;

function TAgentParams.MaxTokens(const Value: Integer): TAgentParams;
begin
  Result := TAgentParams(Add('max_tokens', Value));
end;

function TAgentParams.Messages(
  const Value: TArray<TChatMessagePayload>): TAgentParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TAgentParams(Add('messages', JSONArray));
end;

function TAgentParams.N(const Value: Integer): TAgentParams;
begin
  Result := TAgentParams(Add('n', Value));
end;

function TAgentParams.ParallelToolCalls(const Value: Boolean): TAgentParams;
begin
  Result := TAgentParams(Add('parallel_tool_calls', Value));
end;

function TAgentParams.Prediction(const Value: string): TAgentParams;
begin
  Result := TAgentParams(Add('prediction',
    TJSONObject.Create
      .AddPair('type', 'content')
      .AddPair('content', Value)
  ));
end;

function TAgentParams.PresencePenalty(const Value: Double): TAgentParams;
begin
  Result := TAgentParams(Add('presence_penalty', Value));
end;

function TAgentParams.PromptMode(const Value: string): TAgentParams;
begin
  Result := TAgentParams(Add('prompt_mode', Value));
end;

function TAgentParams.RandomSeed(const Value: Integer): TAgentParams;
begin
  Result := TAgentParams(Add('random_seed', Value));
end;

function TAgentParams.ResponseFormat(const Value: string): TAgentParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('type', Value);
  Result := TAgentParams(Add('response_format', JSON));
end;

function TAgentParams.Stop(const Value: TArray<string>): TAgentParams;
begin
  Result := TAgentParams(Add('stop', Value));
end;

function TAgentParams.Stop(const Value: string): TAgentParams;
begin
  Result := TAgentParams(Add('stop', Value));
end;

function TAgentParams.Stream(const Value: Boolean): TAgentParams;
begin
  Result := TAgentParams(Add('stream', Value));
end;

function TAgentParams.ToolChoice(const Value: TToolChoice): TAgentParams;
begin
  Result := TAgentParams(Add('tool_choice', Value.ToString));
end;

function TAgentParams.ToolChoice(const FunctionName: string): TChatParams;
begin
  var Tool := TJSONParam.Create
        .Add('type', 'function')
        .Add('function', TJSONObject.Create
          .AddPair('Name', FunctionName));
  Result := TChatParams(Add('tool_choice', Tool.Detach));
end;

function TAgentParams.Tools(
  const Value: TArray<TChatMessageTool>): TAgentParams;
var
  Item: TChatMessageTool;
  Items: TJSONArray;
begin
  Items := TJSONArray.Create;
  try
    for Item in Value do
      begin
        Items.Add(Item.ToJson);
      end;
    Result := TAgentParams(Add('tools', Items));
  except
    on E: Exception do
      begin
        Items.Free;
        raise;
      end;
  end;
end;

{ TAgentRoute }

function TAgentRoute.AsyncAwaitCreate(const ParamProc: TProc<TAgentParams>;
  const Callbacks: TFunc<TPromiseChat>): TPromise<TChat>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TChat>(
    procedure(const CallbackParams: TFunc<TAsynChat>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TAgentRoute.AsyncAwaitCreateStream(
  const ParamProc: TProc<TAgentParams>;
  const Callbacks: TFunc<TPromiseChatStream>): TPromise<string>;
begin
  Result := TPromise<string>.Create(
    procedure(Resolve: TProc<string>; Reject: TProc<Exception>)
    var
      Buffer: string;
    begin
      AsyncCreateStream(ParamProc,
        function : TAsynChatStream
        begin
          Result.Sender := Callbacks.Sender;

          Result.OnStart := Callbacks.OnStart;

          Result.OnProgress :=
            procedure (Sender: TObject; Event: TChat)
            begin
              if Assigned(Callbacks.OnProgress) then
                Callbacks.OnProgress(Sender, Event);
              Buffer := Buffer + Event.Choices[0].Delta.Content[0].Text;
            end;

          Result.OnSuccess :=
            procedure (Sender: TObject)
            begin
              Resolve(Buffer);
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

procedure TAgentRoute.AsyncCreate(ParamProc: TProc<TAgentParams>;
  CallBacks: TFunc<TAsynChat>);
begin
  with TAsyncCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentRoute.AsyncCreateStream(ParamProc: TProc<TAgentParams>;
  CallBacks: TFunc<TAsynChatStream>);
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

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
              CreateStream(ParamProc,
                procedure (var Agent: TChat; IsDone: Boolean; var Cancel: Boolean)
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
                  if not IsDone and Assigned(Agent) then
                    begin
                      var LocalAgent := Agent;
                      Agent := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalAgent);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalAgent.Free;
                          end;
                        end);
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

function TAgentRoute.Create(ParamProc: TProc<TAgentParams>): TChat;
begin
  Result := API.Post<TChat, TAgentParams>('agents/completions', ParamProc);
end;

function TAgentRoute.CreateStream(ParamProc: TProc<TAgentParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TAgentParams>('agents/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Agent: TChat;
        TextBuffer: string;
        Line: string;
        Ret: Integer;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        repeat
          Ret := TextBuffer.IndexOf(#10, RetPos);
          if Ret < 0 then
            Continue;
          Line := TextBuffer.Substring(RetPos, Ret - RetPos);
          RetPos := Ret + 1;

          if Line.IsEmpty or Line.StartsWith(#10) then
            Continue;
          Agent := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Agent := TApiDeserializer.Parse<TChat>(
                  TJSONNormalizer.Normalize(Data, ['choices', '*', 'delta', 'content']));
          except
            Agent := nil;
          end;

          try
            Event(Agent, IsDone, AAbort);
          finally
            Agent.Free;
          end;
        until Ret < 0;

      end);
  finally
    Response.Free;
  end;
end;

end.
