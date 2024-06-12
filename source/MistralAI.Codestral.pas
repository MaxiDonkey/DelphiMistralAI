unit MistralAI.Codestral;

{
+------------------------------------------------------------------------------+
|                                                                              |
|  **Important Notice**                                                        |
|                                                                              |
|  To utilize the classes managing the Codestral function, you are required to |
|  create a new KEY on the Mistral.ai website. Please note that obtaining this |
|  key necessitates providing a valid phone number.                            |
|                                                                              |
|  https://console.mistral.ai/codestral                                        |
|                                                                              |
+------------------------------------------------------------------------------+
}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.API.Params, MistralAI.API;

type
  /// <summary>
  /// Finish reason
  /// </summary>
  TCodestralFinishReason = (
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
    error);

  TCodestralFinishReasonHelper = record helper for TCodestralFinishReason
    function ToString: string;
    class function Create(const Value: string): TCodestralFinishReason; static;
  end;

  TCodestralFinishReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TCodestralParams = class(TJSONParam)
    /// <summary>
    /// The text/code to complete
    /// </summary>
    /// <remarks>
    /// Is required
    /// </remarks>
    function Prompt(const Value: string): TCodestralParams;
    /// <summary>
    /// Optional text/code that adds more context for the model. When given a prompt and a suffix the model
    /// will fill what is between them. When suffix is not provided, the model will simply execute completion
    /// starting with prompt
    /// </summary>
    function Suffix(const Value: string): TCodestralParams;
    /// <summary>
    /// ID of the model to use. Only compatible for now with
    /// codestral-2405 or codestral-latest
    /// </summary>
    /// <remarks>
    /// Is required
    /// </remarks>
    function Model(const Value: string): TCodestralParams;
    /// <summary>
    /// Default: 0.7
    /// What sampling temperature to use, between 0.0 and 1.0. Higher values like 0.8 will make the output more
    /// random, while lower values like 0.2 will make it more focused and deterministic.
    /// </summary>
    function Temperature(const Value: Single = 0.7): TCodestralParams;
    /// <summary>
    /// Default: 1
    /// Nucleus sampling, where the model considers the results of the tokens with with top_p probability mass.
    /// So 0.1 means only the tokens comprising the top 10% probability mass are considered
    /// </summary>
    function TopP(const Value: Single = 1): TCodestralParams;
    /// <summary>
    /// The maximum number of tokens to generate in the completion
    /// The token count of your prompt plus max_tokens cannot exceed the model's context length
    /// </summary>
    function MaxTokens(const Value: Integer): TCodestralParams;
    /// <summary>
    /// The minimum number of tokens to generate in the completion
    /// </summary>
    function MinTokens(const Value: Integer): TCodestralParams;
    /// <summary>
    /// Whether to stream back partial progress. If set, tokens will be sent as data-only server-side events
    /// as they become available, with the stream terminated by a data: [DONE] message." Otherwise, the server
    /// will hold the request open until the timeout or until completion, with the response containing the full
    /// result as JSON.
    /// </summary>
    function Stream(const Value: Boolean = True): TCodestralParams;
    /// <summary>
    /// The seed to use for random sampling. If set, different calls will generate deterministic results
    /// </summary>
    function RandomSeed(const Value: Integer): TCodestralParams;
    /// <summary>
    /// Default: []
    /// Stop generation if this token is detected
    /// </summary>
    function Stop(const Value: TArray<string> = []): TCodestralParams;
    constructor Create; override;
  end;

  TCodestralUsage = class
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

  TCodestralMessage = class
  private
    [JsonNameAttribute('role')]
    FRole: string;
    [JsonNameAttribute('content')]
    FContent: string;
  public
    /// <summary>
    /// The role of the author of this message
    /// Enum: "user" "assistant"
    /// </summary>
    property Role: string read FRole write FRole;
    /// <summary>
    /// Response content
    /// </summary>
    property Content: string read FContent write FContent;
  end;

  TCodestralChoices = class
  private
    [JsonNameAttribute('index')]
    FIndex: Int64;
    [JsonNameAttribute('message')]
    FMessage: TCodestralMessage;
    [JsonReflectAttribute(ctString, rtString, TCodestralFinishReasonInterceptor)]
    FFinish_reason: TCodestralFinishReason;
    [JsonNameAttribute('delta')]
    FDelta: TCodestralMessage;
  public
    /// <summary>
    /// The index of the choice in the list of choices
    /// </summary>
    property Index: Int64 read FIndex write FIndex;
    /// <summary>
    /// FIM completion message generated by the model
    /// </summary>
    property Message: TCodestralMessage read FMessage write FMessage;
    /// <summary>
    /// A chat completion delta generated by streamed model responses
    /// </summary>
    property Delta: TCodestralMessage read FDelta write FDelta;
    /// <summary>
    /// The reason the model stopped generating tokens.
    /// This will be stop if the model hit a natural stop point or a provided stop sequence
    /// </summary>
    property FinishReason: TCodestralFinishReason read FFinish_reason write FFinish_reason;
    destructor Destroy; override;
  end;

  TCodestral = class
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
    FChoices: TArray<TCodestralChoices>;
    [JsonNameAttribute('usage')]
    FUsage: TCodestralUsage;
  public
    /// <summary>
    /// A unique identifier for the codestral completion
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always codestral.completion
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The Unix timestamp (in seconds) of when the codestral completion was created
    /// </summary>
    property Created: Int64 read FCreated write FCreated;
    /// <summary>
    /// The model used for the codestral completion
    /// </summary>
    property Model: string read FModel write FModel;
    /// <summary>
    /// A list of codestral completion choices. Can be more than one if N is greater than 1
    /// </summary>
    property Choices: TArray<TCodestralChoices> read FChoices write FChoices;
    /// <summary>
    /// Usage statistics for the completion request
    /// </summary>
    property Usage: TCodestralUsage read FUsage write FUsage;
    destructor Destroy; override;
  end;

  TCodestralEvent = reference to procedure(var Codestral: TCodestral; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// Access to the chat completion API allows you to chat with a model fine-tuned to follow instructions
  /// </summary>
  TCodestralRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Creates a completion for codestral
    /// </summary>
    /// <exception cref="MistralAIExceptionAPI"> MistralAIExceptionAPI </exception>
    /// <exception cref="MistralAIExceptionInvalidRequestError"> MistralAIExceptionInvalidRequestError </exception>
    function Create(ParamProc: TProc<TCodestralParams>): TCodestral;
    /// <summary>
    /// Creates a completion for codestral with streamed response
    /// </summary>
    /// <remarks>
    /// The Chat object will be nil if all data is received
    /// </remarks>
    function CreateStream(ParamProc: TProc<TCodestralParams>; Event: TCodestralEvent): Boolean;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;

{ TCodestralParams }

constructor TCodestralParams.Create;
begin
  inherited;
  Model('codestral-latest');
end;

function TCodestralParams.MaxTokens(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('max_tokens', Value));
end;

function TCodestralParams.MinTokens(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('min_tokens', Value));
end;

function TCodestralParams.Model(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('model', Value));
end;

function TCodestralParams.Prompt(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('prompt', Value));
end;

function TCodestralParams.RandomSeed(const Value: Integer): TCodestralParams;
begin
  Result := TCodestralParams(Add('random_seed', Value));
end;

function TCodestralParams.Stop(const Value: TArray<string>): TCodestralParams;
var
  Items: TJSONArray;
begin
  Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        Items.Add(Item);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TCodestralParams(Add('stop', Items));
end;

function TCodestralParams.Stream(const Value: Boolean): TCodestralParams;
begin
  Result := TCodestralParams(Add('stream', Value));
end;

function TCodestralParams.Suffix(const Value: string): TCodestralParams;
begin
  Result := TCodestralParams(Add('suffix', Value));
end;

function TCodestralParams.Temperature(const Value: Single): TCodestralParams;
begin
  Result := TCodestralParams(Add('temperature', Value));
end;

function TCodestralParams.TopP(const Value: Single): TCodestralParams;
begin
  Result := TCodestralParams(Add('top_p', Value));
end;

{ TCodestralFinishReasonHelper }

class function TCodestralFinishReasonHelper.Create(
  const Value: string): TCodestralFinishReason;
begin
  case IndexStr(AnsiLowerCase(Value), ['stop', 'length', 'model_length', 'error']) of
    0 :
      Exit(stop);
    1 :
      Exit(length);
    2 :
      Exit(model_length);
    3 :
      Exit(error);
  end;
  Result := stop;
end;

function TCodestralFinishReasonHelper.ToString: string;
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
  end;
end;

{ TCodestralFinishReasonInterceptor }

function TCodestralFinishReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TCodestralFinishReason>.ToString;
end;

procedure TCodestralFinishReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TCodestralFinishReason.Create(Arg)));
end;

{ TCodestralChoices }

destructor TCodestralChoices.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TCodestral }

destructor TCodestral.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  for var Item in FChoices do
    Item.Free;
  inherited;
end;

{ TCodestralRoute }

function TCodestralRoute.Create(
  ParamProc: TProc<TCodestralParams>): TCodestral;
begin
  Result := API.Post<TCodestral, TCodestralParams>('fim/completions', ParamProc);
end;

function TCodestralRoute.CreateStream(ParamProc: TProc<TCodestralParams>;
  Event: TCodestralEvent): Boolean;
var
  Response: TStringStream;
  RetPos: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    RetPos := 0;
    Result := API.Post<TCodestralParams>('fim/completions', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Code: TCodestral;
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
          Code := nil;
          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          IsDone := Data = '[DONE]';

          if not IsDone then
          try
            Code := TJson.JsonToObject<TCodestral>(Data);
          except
            Code := nil;
          end;

          try
            Event(Code, IsDone, AAbort);
          finally
            Code.Free;
          end;
        until Ret < 0;

      end);
  finally
    Response.Free;
  end;
end;

end.
