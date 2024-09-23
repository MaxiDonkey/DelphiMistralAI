unit MistralAI.Agents;

{+-----------------------------------------------------------------------------+
 |     Github repository : https://github.com/MaxiDonkey/DelphiMistralAI       |
 |     Visit the Github repository for the documentation and use examples      |
 +-----------------------------------------------------------------------------+}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, MistralAI.API.Params, MistralAI.API, MistralAI.Functions.Core,
  MistralAI.Functions.Tools, MistralAI.Chat, MistralAI.Vision.Params, MistralAI.Params.Core,
  MistralAI.Async.Support;

type
  /// <summary>
  /// Represents a function call within a tool, including the function's name and arguments.
  /// </summary>
  /// <remarks>
  /// Used to specify the function that a tool should execute along with its arguments.
  /// </remarks>
  TToolCallFunction = record
  private
    FName: string;
    FArguments: string;
  public
    /// <summary>
    /// Name of the function.
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Arguments to be used when running the function.
    /// </summary>
    property Arguments: string read FArguments write FArguments;
    /// <summary>
    /// Initializes a new instance of <c>TToolCallFunction</c> with the specified name and arguments.
    /// </summary>
    /// <param name="Name">
    /// The name of the function.
    /// </param>
    /// <param name="Arguments">
    /// The arguments to be used when running the function.
    /// </param>
    /// <returns>
    /// A new instance of <c>TToolCallFunction</c> initialized with the specified name and arguments.
    /// </returns>
    class function Create(Name, Arguments: string): TToolCallFunction; static;
  end;

  /// <summary>
  /// Represents a tool call, including its ID, type, and the function to be executed.
  /// </summary>
  /// <remarks>
  /// Used to define a call to a specific function within a tool, identified by an ID.
  /// </remarks>
  TToolCall = record
  private
    FId: string;
    FType: string;
    FFunction: TToolCallFunction;
  public
    /// <summary>
    /// The identifier of the tool call.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The type of the tool call. Default is "function".
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// The function to be executed in the tool call.
    /// </summary>
    property &Function: TToolCallFunction read FFunction write FFunction;
    /// <summary>
    /// Initializes a new instance of <c>TToolCall</c> with the specified ID, function name, and arguments.
    /// </summary>
    /// <param name="Id">
    /// The identifier for the tool call.
    /// </param>
    /// <param name="Name">
    /// The name of the function to be called.
    /// </param>
    /// <param name="Arguments">
    /// The arguments to be passed to the function.
    /// </param>
    /// <returns>
    /// A new instance of <c>TToolCall</c> initialized with the specified ID, name, and arguments.
    /// </returns>
    class function Create(Id, Name, Arguments: string): TToolCall; static;
  end;

  /// <summary>
  /// Represents a message payload for an agent, including role, content, and additional properties.
  /// </summary>
  /// <remarks>
  /// Used to structure messages exchanged with the model, supporting roles like user, assistant, and tool.
  /// </remarks>
  TAgentMessagePayload = record
  private
    FRole: TMessageRole;
    FContent: string;
    FName: string;
    FToolCallId: string;
    FPrefix: Boolean;
    FToolCall: TArray<TToolCall>;
    FVisionSources: TArray<TVisionSource>;
  public
    /// <summary>
    /// The role of the message. Value chosen between “user”, “assistant”, or “tool”.
    /// </summary>
    property Role: TMessageRole read FRole write FRole;
    /// <summary>
    /// The contents of the message
    /// </summary>
    property Content: string read FContent write FContent;

    /// <summary>
    /// Represents an array of <c>TVisionSource</c> objects that contain image sources for the vision system.
    /// Each <c>TVisionSource</c> object stores the image either as a URL or a Base64-encoded string.
    /// </summary>
    /// <value>
    /// A dynamic array (<c>TArray</c>) of <c>TVisionSource</c> objects. This array holds the image sources used in the vision system.
    /// </value>
    /// <remarks>
    /// The <c>TVisionSource</c> objects in this array can be used to provide images to the system. Each source is verified as a secure HTTPS URL or a Base64-encoded image.
    /// </remarks>
    property VisionSources: TArray<TVisionSource> read FVisionSources write FVisionSources;

    /// <summary>
    /// The name of the tool when the role is set to "tool".
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// The identifier of the tool call when the role is set to "tool".
    /// </summary>
    property ToolCallId: string read FToolCallId write FToolCallId;

    /// <summary>
    /// Set this to true when adding an assistant message as a prefix to condition the model's response.
    /// The role of the prefix message is to ensure that the model begins its answer with the content of
    /// the message. Applies to role = assistant.
    /// </summary>
    property Prefix: Boolean read FPrefix write FPrefix;
    /// <summary>
    /// An array of tool calls (<c>TToolCall</c>) or null when the role is "assistant".
    /// </summary>
    property ToolCall: TArray<TToolCall> read FToolCall write FToolCall;

    /// <summary>
    /// Initializes a new instance of <c>TAgentMessagePayload</c> with the role of "assistant".
    /// </summary>
    /// <param name="Content">
    /// The content of the assistant's message.
    /// </param>
    /// <param name="ToolCall">
    /// An array of tool calls associated with the assistant's message.
    /// </param>
    /// <param name="Prefix">
    /// Optional. If set to true, the message is used as a prefix to condition the model's response.
    /// </param>
    /// <returns>
    /// A new instance of <c>TAgentMessagePayload</c> with the role set to "assistant" and initialized with the specified parameters.
    /// </returns>
    class function Assistant(const Content: string; const ToolCall: TArray<TToolCall>;
      const Prefix: Boolean = False): TAgentMessagePayload; static;
    /// <summary>
    /// Initializes a new instance of <c>TAgentMessagePayload</c> with the role of "tool".
    /// </summary>
    /// <param name="Content">
    /// The content of the tool's message.
    /// </param>
    /// <param name="Name">
    /// The name of the tool.
    /// </param>
    /// <param name="ToolCallId">
    /// The identifier of the tool call.
    /// </param>
    /// <returns>
    /// A new instance of <c>TAgentMessagePayload</c> with the role set to "tool" and initialized with the specified parameters.
    /// </returns>
    class function Tool(const Content, Name, ToolCallId: string): TAgentMessagePayload; static;
    /// <summary>
    /// Initializes a new instance of <c>TAgentMessagePayload</c> with the role of "user".
    /// </summary>
    /// <param name="Content">
    /// The content of the user's message.
    /// </param>
    /// <returns>
    /// A new instance of <c>TAgentMessagePayload</c> with the role set to "user" and the specified content.
    /// </returns>
    class function User(const Content: string): TAgentMessagePayload; overload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the user and includes associated vision sources.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="VisionSrc">
    /// An array of strings representing vision sources.
    /// </param>
    /// <returns>
    /// A new instance of <c>TAgentMessagePayload</c> with the role set to "user", including the specified content and vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be URLs or Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function User(const Content: string;
      const VisionSrc: TArray<string>): TAgentMessagePayload; overload; static;
  end;

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
    /// Sets the minimum number of tokens to generate in the completion.
    /// </summary>
    /// <param name="Value">
    /// The minimum number of tokens to generate in the completion.
    /// </param>
    /// <returns>
    /// An instance of <c>TAgentParams</c> with the <c>MinTokens</c> parameter set.
    /// </returns>
    function MinTokens(const Value: Integer): TAgentParams;
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
    function Messages(const Value: TArray<TAgentMessagePayload>): TAgentParams;
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
    function ToolChoice(const Value: TToolChoice = auto): TAgentParams;
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
  /// Alias for <c>TChat</c>, representing an agent in the system.
  /// </summary>
  TAgent = TChat;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TAgent</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynAgentParams</c> type extends the <c>TAsynParams&lt;TAgent&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynAgentParams = TAsyncCallBack<TAgent>;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TAgentt</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynAgentStreamParams</c> type extends the <c>TAsynStreamParams&lt;TAgent&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynAgentStreamParams = TAsyncStreamCallBack<TAgent>;

  /// <summary>
  /// A callback type for handling streamed agent responses during asynchronous operations.
  /// </summary>
  /// <param name="Agent">
  /// The <c>TAgent</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// Implement this procedure to process each piece of data received during streaming.
  /// Set <c>Cancel</c> to <c>True</c> to terminate the streaming early.
  /// </remarks>
  TAgentEvent = reference to procedure(var Agent: TAgent; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Provides methods to interact with agents, including creating completions and handling asynchronous operations.
  /// </summary>
  /// <remarks>
  /// Use this class to manage agent interactions, including synchronous and asynchronous requests.
  /// </remarks>
  TAgentRoute = class(TMistralAIAPIRoute)
  public
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
    /// AgentRoute.AsyncCreate(
    ///   procedure(Params: TAgentParams)
    ///   begin
    ///     Params.AgentId('agent_id').Messages(MessagesArray);
    ///   end,
    ///   function: TAsynAgentParams
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
    procedure AsyncCreate(ParamProc: TProc<TAgentParams>; CallBacks: TFunc<TAsynAgentParams>);

    /// <summary>
    /// Creates an asynchronous streaming agent completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the agent request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynAgentStreamParams</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// // Example usage:
    /// AgentRoute.AsyncStreamCreate(
    ///   procedure(Params: TAgentParams)
    ///   begin
    ///     Params.AgentId('agent_id').Messages(MessagesArray);
    ///   end,
    ///   function: TAsynAgentParams
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
    ///          Result := CheckBox1.Checked;
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreateStream(ParamProc: TProc<TAgentParams>;
      CallBacks: TFunc<TAsynAgentStreamParams>);

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
    /// </remarks>
    function Create(ParamProc: TProc<TAgentParams>): TAgent;
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
    /// </remarks>
    function CreateStream(ParamProc: TProc<TAgentParams>; Event: TAgentEvent): Boolean;
  end;

implementation

uses
  system.StrUtils, Rest.Json;

{ TAgentParams }

function TAgentParams.AgentId(const Value: string): TAgentParams;
begin
  Result := TAgentParams(Add('agent_id', Value));
end;

function TAgentParams.MaxTokens(const Value: Integer): TAgentParams;
begin
  Result := TAgentParams(Add('max_tokens', Value));
end;

function TAgentParams.Messages(
  const Value: TArray<TAgentMessagePayload>): TAgentParams;
var
  JSON: TJSONObject;
begin
  var Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        var SubItems: TJSONArray := nil;

        if Length(Item.FVisionSources) > 0 then
          {--- Processing with images }
          begin
            SubItems := TJSONArray.Create;

            JSON := TJSONObject.Create;
            {"type": "text", "text": "What’s in this image?"}
            JSON.AddPair('type', 'text');
            JSON.AddPair('text', Item.Content);
            SubItems.Add(JSON);

            for var Source in Item.VisionSources do
              begin
                JSON := TJSONObject.Create;
                {"type": "image_url", "image_url": "Url or Image content to base64 string"}
                JSON.AddPair('type', 'image_url');
                JSON.AddPair('image_url', Source.Data);
                SubItems.Add(JSON);
              end;
          end;

        JSON := TJSONObject.Create;
        {--- Add role }
        JSON.AddPair('role', Item.Role.ToString);
        {--- Add content }
        if Length(Item.FVisionSources) > 0 then
          JSON.AddPair('content', SubItems) else
          JSON.AddPair('content', Item.Content);

        case Item.Role of
          tool: begin
            {--- Add tool_call_id }
            if Item.ToolCallId <> EmptyStr then
              JSON.AddPair('tool_call_id', Item.ToolCallId);
            {--- Add name }
            if Item.Name <> EmptyStr then
              JSON.AddPair('name', Item.Name);
          end;

          assistant: begin
            {--- Add Prefix }
            if Item.Prefix then
              JSON.AddPair('prefix', True);

            var ToolCallJSONArray := TJSONArray.Create;
            for var Data in Item.ToolCall do
              begin
                var JSONToolCall := TJSONObject.Create;
                {--- Add tool_calls.id }
                JSONToolCall.AddPair('id', Data.Id);
                {--- Add tool_calls.type }
                JSONToolCall.AddPair('type', Data.&Type);

                  var JSONFunction := TJSONObject.Create;
                  {--- Add tool_calls.function.name }
                  JSONFunction.AddPair('name', Data.&Function.Name);
                  {--- Add tool_calls.function.arguments }
                  JSONFunction.AddPair('arguments', Data.&Function.Arguments);

                {--- Add tool_calls.function }
                JSONToolCall.AddPair('function', JSONFunction);

                ToolCallJSONArray.Add(JSONToolCall);
              end;

            {--- Add tool_calls }
            JSON.AddPair('tool_calls', ToolCallJSONArray);
          end;
        end;

        Items.Add(JSON);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TAgentParams(Add('messages', Items));
end;

function TAgentParams.MinTokens(const Value: Integer): TAgentParams;
begin
  Result := TAgentParams(Add('min_tokens', Value));
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

{ TAgentMessagePayload }

class function TAgentMessagePayload.Assistant(const Content: string;
  const ToolCall: TArray<TToolCall>; const Prefix: Boolean = False): TAgentMessagePayload;
begin
  Result.FRole := TMessageRole.assistant;
  Result.FContent := Content;
  Result.FPrefix := Prefix;
  Result.FToolCall := ToolCall;
end;

class function TAgentMessagePayload.Tool(const Content, Name,
  ToolCallId: string): TAgentMessagePayload;
begin
  Result.FRole := TMessageRole.tool;
  Result.FContent := Content;
  Result.Name := Name;
  Result.ToolCallId := ToolCallId;
end;

class function TAgentMessagePayload.User(const Content: string;
  const VisionSrc: TArray<string>): TAgentMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
  for var Item in VisionSrc do
    Result.VisionSources := Result.VisionSources + [TVisionSource.Create(Item)];
end;

class function TAgentMessagePayload.User(const Content: string): TAgentMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
end;

{ TAgentRoute }

procedure TAgentRoute.AsyncCreate(ParamProc: TProc<TAgentParams>;
  CallBacks: TFunc<TAsynAgentParams>);
begin
  with TAsyncCallBackExec<TAsynAgentParams, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentRoute.AsyncCreateStream(ParamProc: TProc<TAgentParams>;
  CallBacks: TFunc<TAsynAgentStreamParams>);
begin
  var CallBackParams := TUseParamsFactory<TAsynAgentStreamParams>.CreateInstance(CallBacks);

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
                procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
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
                  if not IsDone and Assigned(Chat) then
                    begin
                      var LocalChat := Chat;
                      Chat := nil;

                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChat);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
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

function TAgentRoute.Create(ParamProc: TProc<TAgentParams>): TAgent;
begin
  Result := API.Post<TAgent, TAgentParams>('agents/completions', ParamProc);
end;

function TAgentRoute.CreateStream(ParamProc: TProc<TAgentParams>;
  Event: TAgentEvent): Boolean;
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
        Agent: TAgent;
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
            Agent := TJson.JsonToObject<TAgent>(Data);
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

{ TToolCall }

class function TToolCall.Create(Id, Name, Arguments: string): TToolCall;
begin
  Result.Id := Id;
  Result.&Type := 'function';
  Result.&Function := TToolCallFunction.Create(Name, Arguments);
end;

{ TToolCallFunction }

class function TToolCallFunction.Create(Name,
  Arguments: string): TToolCallFunction;
begin
  Result.Name := Name;
  Result.Arguments := Arguments;
end;

end.
