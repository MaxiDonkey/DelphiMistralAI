unit MistralAI.Chat;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Functions.Core,
  MistralAI.Functions.Tools;

type
  /// <summary>
  /// Type of message role
  /// </summary>
  TMessageRole = (
    /// <summary>
    /// System message
    /// </summary>
    system,
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    assistant,
    /// <summary>
    /// Function message
    /// </summary>
    tool);

  TMessageRoleHelper = record helper for TMessageRole
    function ToString: string;
    class function FromString(const Value: string): TMessageRole; static;
  end;

  /// <summary>
  /// Finish reason
  /// </summary>
  TFinishReason = (
    /// <summary>
    /// API returned complete model output
    /// </summary>
    stop,
    /// <summary>
    /// Incomplete model output due to max_tokens parameter or token limit
    /// </summary>
    length,
    /// <summary>
    /// model_length
    /// </summary>
    model_length,
    /// <summary>
    /// An error was encountered while processing the request
    /// </summary>
    error,
    /// <summary>
    /// A function must be invoked before further processing of the request
    /// </summary>
    tool_calls);

  TFinishReasonHelper = record helper for TFinishReason
    function ToString: string;
    class function Create(const Value: string): TFinishReason; static;
  end;

  TFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TChatMessagePayload = record
  private
    FRole: TMessageRole;
    FContent: string;
  public
    /// <summary>
    /// The role of the message. Value chosen between “user” and “assistant”
    /// </summary>
    property Role: TMessageRole read FRole write FRole;
    /// <summary>
    /// The contents of the message. content is required for all messages
    /// </summary>
    property Content: string read FContent write FContent;
    /// <summary>
    /// Create a new "payload" message with assistant role
    /// </summary>
    class function Assistant(const Content: string): TChatMessagePayload; static;
    /// <summary>
    /// Create a new "payload" message with system role
    /// </summary>
    class function System(const Content: string): TChatMessagePayload; static;
    /// <summary>
    /// Create a new "payload" message with user role
    /// </summary>
    class function User(const Content: string): TChatMessagePayload; static;
  end;

  TChatParams = class(TJSONParam)
    /// <summary>
    /// ID of the model to use. You can use the List Available Models API to see all of your available models
    /// </summary>
    /// <seealso>https://docs.mistral.ai/models/</seealso>
    function Model(const Value: string): TChatParams;
    /// <summary>
    /// The maximum number of tokens to generate in the completion.
    /// The token count of your prompt plus max_tokens cannot exceed the model's context length
    /// </summary>
    function MaxTokens(const Value: Integer = 16): TChatParams;
    /// <summary>
    /// The prompt(s) to generate completions for, encoded as a list of dict with role and content
    /// The first prompt role should be user or system.
    /// </summary>
    function Messages(const Value: TArray<TChatMessagePayload>): TChatParams;
    /// <summary>
    /// An object specifying the format that the model must output. Setting to { "type": "json_object" }
    /// enables JSON mode, which guarantees the message the model generates is in JSON.
    /// When using JSON mode you MUST also instruct the model to produce JSON yourself with a system or
    /// a user message
    /// </summary>
    /// <remarks>
    /// Default value { "type": "text" } if ResponseFormat not called
    /// </remarks>
    function ResponseFormat(const Value: string = 'json_object'): TChatParams;
    /// <summary>
    /// Whether to stream back partial progress. If set, tokens will be sent as data-only server-sent events as they become available,
    /// with the stream terminated by a data: [DONE] message. Otherwise, the server will hold the request open until the timeout or
    /// until completion, with the response containing the full result as JSON
    /// </summary>
    function Stream(const Value: Boolean = True): TChatParams;
    /// <summary>
    /// What sampling temperature to use, between 0.0 and 1.0. Higher values like 0.8 will make the output more random,
    /// while lower values like 0.2 will make it more focused and deterministic
    /// </summary>
    function Temperature(const Value: Single = 0.7): TChatParams;
    /// <summary>
    /// Nucleus sampling, where the model considers the results of the tokens with top_p probability mass.
    /// So 0.1 means only the tokens comprising the top 10% probability mass are considered
    /// </summary>
    function TopP(const Value: Single = 1): TChatParams;
    /// <summary>
    /// Whether to inject a safety prompt before all conversations.
    /// </summary>
    function SafePrompt(const Value: Boolean = False): TChatParams;
    /// <summary>
    /// A list of available tools for the model. Use this to specify functions for which the model can generate JSON inputs
    /// </summary>
    function Tools(const Value: TArray<TChatMessageTool>): TChatParams;
    /// <summary>
    /// Specifies if/how functions are called. If set to none the model won't call a function and will generate a message instead.
    /// If set to auto the model can choose to either generate a message or call a function. If set to any the model is forced
    /// to call a function
    /// Default: "auto"
    /// </summary>
    function ToolChoice(const Value: TToolChoice = auto): TChatParams;
    /// <summary>
    /// The seed to use for random sampling. If set, different calls will generate deterministic results
    /// </summary>
    function RandomSeed(const Value: Integer): TChatParams;
    constructor Create; override;
  end;

  TChatUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPrompt_tokens: Int64;
    [JsonNameAttribute('completion_tokens')]
    FCompletion_tokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotal_tokens: Int64;
  public
    /// <summary>
    /// Number of tokens in the prompt
    /// </summary>
    property PromptTokens: Int64 read FPrompt_tokens write FPrompt_tokens;
    /// <summary>
    /// Number of tokens in the generated completion
    /// </summary>
    property CompletionTokens: Int64 read FCompletion_tokens write FCompletion_tokens;
    /// <summary>
    /// Total number of tokens used in the request (prompt + completion)
    /// </summary>
    property TotalTokens: Int64 read FTotal_tokens write FTotal_tokens;
  end;

  TChatMessage = class
  private
    [JsonNameAttribute('role')]
    FRole: string;
    [JsonNameAttribute('content')]
    FContent: string;
    [JsonNameAttribute('tool_calls')]
    FToolsCalls: TArray<TCalledFunction>;
  public
    /// <summary>
    /// The role of the author of this message
    /// </summary>
    property Role: string read FRole write FRole;
    /// <summary>
    /// The contents of the message
    /// </summary>
    property Content: string read FContent write FContent;
    /// <summary>
    /// "tool calls" must be runned for query complement
    /// </summary>
    property ToolsCalls: TArray<TCalledFunction> read FToolsCalls write FToolsCalls;
    destructor Destroy; override;
  end;

  TChatChoices = class
  private
    [JsonNameAttribute('index')]
    FIndex: Int64;
    [JsonNameAttribute('message')]
    FMessage: TChatMessage;
    [JsonReflectAttribute(ctString, rtString, TFinishReasonInterceptor)]
    FFinish_reason: TFinishReason;
    [JsonNameAttribute('delta')]
    FDelta: TChatMessage;
  public
    /// <summary>
    /// The index of the choice in the list of choices
    /// </summary>
    property Index: Int64 read FIndex write FIndex;
    /// <summary>
    /// A chat completion message generated by the model
    /// </summary>
    property Message: TChatMessage read FMessage write FMessage;
    /// <summary>
    /// A chat completion delta generated by streamed model responses
    /// </summary>
    property Delta: TChatMessage read FDelta write FDelta;
    /// <summary>
    /// The reason the model stopped generating tokens.
    /// This will be stop if the model hit a natural stop point or a provided stop sequence
    /// </summary>
    property FinishReason: TFinishReason read FFinish_reason write FFinish_reason;
    destructor Destroy; override;
  end;

  TChat = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('created')]
    FCreated: Int64;
    [JsonNameAttribute('model')]
    FModel: string;
    [JsonNameAttribute('choices')]
    FChoices: TArray<TChatChoices>;
    [JsonNameAttribute('usage')]
    FUsage: TChatUsage;
  public
    /// <summary>
    /// A unique identifier for the chat completion
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always chat.completion
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The Unix timestamp (in seconds) of when the chat completion was created
    /// </summary>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// The model used for the chat completion
    /// </summary>
    property Model: string read FModel write FModel;
    /// <summary>
    /// A list of chat completion choices. Can be more than one if N is greater than 1
    /// </summary>
    property Choices: TArray<TChatChoices> read FChoices write FChoices;
    /// <summary>
    /// Usage statistics for the completion request
    /// </summary>
    property Usage: TChatUsage read FUsage write FUsage;
    destructor Destroy; override;
  end;

  TChatEvent = reference to procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Access to the chat completion API allows you to chat with a model fine-tuned to follow instructions
  /// </summary>
  TChatRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Creates a completion for the chat message
    /// </summary>
    /// <exception cref="MistralAIExceptionAPI"> MistralAIExceptionAPI </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError"> MistralAIExceptionInvalidRequestError </exception>
    function Create(ParamProc: TProc<TChatParams>): TChat;
    /// <summary>
    /// Creates a completion for the chat message with streamed response
    /// </summary>
    /// <remarks>
    /// The Chat object will be nil if all data is received
    /// </remarks>
    function CreateStream(ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
  end;

implementation

uses
  system.StrUtils, Rest.Json, System.Rtti;

{ TMessageRoleHelper }

class function TMessageRoleHelper.FromString(const Value: string): TMessageRole;
begin
  case IndexStr(AnsiLowerCase(Value), ['system', 'user', 'assistant', 'tool']) of
    0 :
      Exit(system);
    1 :
      Exit(user);
    2 :
      Exit(assistant);
    3 :
      Exit(tool);
  end;
  Result := user;
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    system:
      Exit('system');
    user:
      Exit('user');
    assistant:
      Exit('assistant');
    tool:
      Exit('tool');
  end;
end;

{ TChatParams }

constructor TChatParams.Create;
begin
  inherited;
  Model('mistral-tiny');
end;

function TChatParams.MaxTokens(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_tokens', Value));
end;

function TChatParams.Messages(
  const Value: TArray<TChatMessagePayload>): TChatParams;
var
  Item: TChatMessagePayload;
  JSON: TJSONObject;
  Items: TJSONArray;
begin
  Items := TJSONArray.Create;
  try
    for Item in Value do
      begin
        JSON := TJSONObject.Create;
        {--- Add role }
        JSON.AddPair('role', Item.Role.ToString);

        {--- Add content }
        if not Item.Content.IsEmpty then
          JSON.AddPair('content', Item.Content);
        Items.Add(JSON);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TChatParams(Add('messages', Items));
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.RandomSeed(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('random_seed', Value));
end;

function TChatParams.ResponseFormat(const Value: string): TChatParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('type', Value);
  Result := TChatParams(Add('response_format', JSON));
end;

function TChatParams.SafePrompt(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('safe_prompt', Value));
end;

function TChatParams.Stream(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('stream', Value));
end;

function TChatParams.Temperature(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('temperature', Value));
end;

function TChatParams.ToolChoice(const Value: TToolChoice): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', Value.ToString));
end;

function TChatParams.Tools(const Value: TArray<TChatMessageTool>): TChatParams;
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
    Result := TChatParams(Add('tools', Items));
  except
    on E: Exception do
      begin
        Items.Free;
        raise;
      end;
  end;
end;

function TChatParams.TopP(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_p', Value));
end;

{ TChatMessagePayload }

class function TChatMessagePayload.Assistant(
  const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.assistant;
  Result.FContent := Content;
end;

class function TChatMessagePayload.System(const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.system;
  Result.FContent := Content;
end;

class function TChatMessagePayload.User(
  const Content: string): TChatMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
end;

{ TFinishReasonHelper }

class function TFinishReasonHelper.Create(const Value: string): TFinishReason;
begin
  case IndexStr(AnsiLowerCase(Value), ['stop', 'length', 'model_length', 'error', 'tool_calls']) of
    0 :
      Exit(stop);
    1 :
      Exit(length);
    2 :
      Exit(model_length);
    3 :
      Exit(error);
    4 :
      Exit(tool_calls);
  end;
  Result := stop;
end;

function TFinishReasonHelper.ToString: string;
begin
  case Self of
    stop:
      Exit('stop');
    length:
      Exit('length');
    model_length:
      Exit('model_length');
    error:
      Exit('error');
    tool_calls:
      Exit('tool_calls');
  end;
end;

{ TFinishReasonInterceptor }

function TFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFinishReason>.ToString;
end;

procedure TFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFinishReason.Create(Arg)));
end;

{ TChatChoices }

destructor TChatChoices.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TChat }

destructor TChat.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  for var Item in FChoices do
    Item.Free;
  inherited;
end;

{ TChatRoute }

function TChatRoute.Create(ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('chat/completions', ParamProc);
end;

function TChatRoute.CreateStream(ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TChatParams>('chat/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Chat: TChat;
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
          Chat := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Chat := TJson.JsonToObject<TChat>(Data);
          except
            Chat := nil;
          end;

          try
            Event(Chat, IsDone, AAbort);
          finally
            Chat.Free;
          end;
        until Ret < 0;

      end);
  finally
    Response.Free;
  end;
end;

{ TChatMessage }

destructor TChatMessage.Destroy;
begin
  for var Tool in FToolsCalls do
    Tool.Free;
  inherited;
end;


end.
