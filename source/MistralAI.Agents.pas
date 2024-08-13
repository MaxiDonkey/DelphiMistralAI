unit MistralAI.Agents;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API, MistralAI.Functions.Core,
  MistralAI.Functions.Tools, MistralAI.Chat;

type
  TToolCallFunction = record
  private
    FName: string;
    FArguments: string;
  public
    /// <summary>
    /// Name of the function
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Arguments to be used when running the function
    /// </summary>
    property Arguments: string read FArguments write FArguments;
    /// <summary>
    /// Initialize a new record "TToolCallFunction" with the values provided in the given parameters
    /// </summary>
    class function Create(Name, Arguments: string): TToolCallFunction; static;
  end;

  TToolCall = record
  private
    FId: string;
    FType: string;
    FFunction: TToolCallFunction;
  public
    /// <summary>
    /// The Id of the function
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// string (ToolTypes)
    /// Default: "function"
    /// Value: "function"
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// object (FunctionCall)
    /// </summary>
    property &Function: TToolCallFunction read FFunction write FFunction;
    /// <summary>
    /// Initialize a new record "TToolCall" with the values provided in the given parameters
    /// </summary>
    class function Create(Id, Name, Arguments: string): TToolCall; static;
  end;

  TAgentMessagePayload = record
  private
    FRole: TMessageRole;
    FContent: string;
    FName: string;
    FToolCallId: string;
    FPrefix: Boolean;
    FToolCall: TArray<TToolCall>;
  public
    /// <summary>
    /// The role of the message. Value chosen between “user” and “assistant”
    /// </summary>
    property Role: TMessageRole read FRole write FRole;
    /// <summary>
    /// The contents of the message
    /// </summary>
    property Content: string read FContent write FContent;

    /// <summary>
    /// This is the Name of the Tool for role = tool
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Tool Call Id (string) or null for role = tool
    /// </summary>
    property ToolCallId: string read FToolCallId write FToolCallId;

    /// <summary>
    /// Set this to true when adding an assistant message as a prefix to condition the model's response.
    /// The role of the prefix message is to ensure that the model begins its answer with the content of
    /// the message. Applies to role = assistant.
    /// </summary>
    property Prefix: Boolean read FPrefix write FPrefix;
    /// <summary>
    /// An array of Tool Calls (TToolCall) or null for the role of assistant
    /// </summary>
    property ToolCall: TArray<TToolCall> read FToolCall write FToolCall;

    /// <summary>
    /// Initialize a new record "TAgentMessagePayload" with the values provided in the given parameters
    /// with "assistant" role
    /// </summary>
    class function Assistant(const Content: string; const ToolCall: TArray<TToolCall>;
      const Prefix: Boolean = False): TAgentMessagePayload; static;
    /// <summary>
    /// Initialize a new record "TAgentMessagePayload" with the values provided in the given parameters
    /// with "tool" role
    /// </summary>
    class function Tool(const Content, Name, ToolCallId: string): TAgentMessagePayload; static;
    /// <summary>
    /// Initialize a new record "TAgentMessagePayload" with the values provided in the given parameters
    /// with "user" role
    /// </summary>
    class function User(const Content: string): TAgentMessagePayload; static;
  end;

  TAgentParams = class(TJSONParam)
    /// <summary>
    /// Max Tokens (integer) or Max Tokens (null) (Max Tokens)
    /// The maximum number of tokens to generate in the completion.
    /// The token count of your prompt plus max_tokens cannot exceed the model's context length
    /// </summary>
    function MaxTokens(const Value: Integer): TAgentParams;
    /// <summary>
    /// Min Tokens (integer) or Min Tokens (null) (Min Tokens)
    /// The minimum number of tokens to generate in the completion
    /// </summary>
    function MinTokens(const Value: Integer): TAgentParams;
    /// <summary>
    /// boolean (Stream)
    /// Default: false
    /// Whether to stream back partial progress. If set, tokens will be sent as data-only
    /// server-side events as they become available, with the stream terminated by a data:
    ///  [DONE] message.
    /// Otherwise, the server will hold the request open until the timeout or until completion,
    /// with the response containing the full result as JSON
    /// </summary>
    function Stream(const Value: Boolean = True): TAgentParams;
    /// <summary>
    /// Stop (string)
    /// Stop generation if this token is detected.
    /// </summary>
    function Stop(const Value: string): TAgentParams; overload;
    /// <summary>
    /// Array of Stop (strings) (Stop)
    /// Stop if one of these tokens is detected when providing an array
    /// </summary>
    function Stop(const Value: TArray<string>): TAgentParams; overload;
    /// <summary>
    /// Random Seed (integer) or Random Seed (null) (Random Seed)
    /// The seed to use for random sampling. If set, different calls will generate deterministic results
    /// </summary>
    function RandomSeed(const Value: Integer): TAgentParams;
    /// <summary>
    /// Array of any (TAgentMessagePayload)
    /// required
    /// The prompt(s) to generate completions for, encoded as a list of dict with role and content
    /// </summary>
    function Messages(const Value: TArray<TAgentMessagePayload>): TAgentParams;
    /// <summary>
    /// An object specifying the format that the model must output. Setting to { "type": "json_object" }
    /// enables JSON mode, which guarantees the message the model generates is in JSON. When using JSON
    /// mode you MUST also instruct the model to produce JSON yourself with a system or a user message
    /// </summary>
    /// <remarks>
    /// Default value { "type": "text" } if ResponseFormat not called
    /// </remarks>
    function ResponseFormat(const Value: string = 'json_object'): TAgentParams;
    /// <summary>
    /// A list of available tools for the model. Use this to specify functions for which the model can generate JSON inputs
    /// </summary>
    function Tools(const Value: TArray<TChatMessageTool>): TAgentParams;
    /// <summary>
    /// Specifies if/how functions are called. If set to none the model won't call a function and will generate a message instead.
    /// If set to auto the model can choose to either generate a message or call a function. If set to any the model is forced
    /// to call a function
    /// Default: "auto"
    /// </summary>
    function ToolChoice(const Value: TToolChoice = auto): TAgentParams;
    /// <summary>
    /// Required
    /// The ID of the agent to use for this completion
    /// </summary>
    function AgentId(const Value: string): TAgentParams;
  end;

  TAgent = TChat;

  TAgentEvent = reference to procedure(var Agent: TAgent; IsDone: Boolean; var Cancel: Boolean);

  TAgentRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Creates a completion for one Agent
    /// </summary>
    /// <exception cref="MistralAIExceptionAPI"> MistralAIExceptionAPI </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError"> MistralAIExceptionInvalidRequestError </exception>
    function Create(ParamProc: TProc<TAgentParams>): TAgent;
     /// <summary>
    /// Creates a completion for one Agent with streamed response
    /// </summary>
    /// <remarks>
    /// The Agent object will be nil if all data is received
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
  Item: TAgentMessagePayload;
  JSON: TJSONObject;
  Items: TJSONArray;
  Data: TToolCall;
  ToolCallJSONArray: TJSONArray;
  JSONToolCall: TJSONObject;
  JSONFunction: TJSONObject;
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

            ToolCallJSONArray := TJSONArray.Create;
            for Data in Item.ToolCall do
              begin
                JSONToolCall := TJSONObject.Create;
                {--- Add tool_calls.id }
                JSONToolCall.AddPair('id', Data.Id);
                {--- Add tool_calls.type }
                JSONToolCall.AddPair('type', Data.&Type);

                  JSONFunction := TJSONObject.Create;
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

class function TAgentMessagePayload.User(const Content: string): TAgentMessagePayload;
begin
  Result.FRole := TMessageRole.user;
  Result.FContent := Content;
end;

{ TAgentRoute }

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
