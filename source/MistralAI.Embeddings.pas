unit MistralAI.Embeddings;

interface

uses
  System.SysUtils, REST.Json.Types, MistralAI.API.Params, MistralAI.API;

type
  TEmbeddingParams = class(TJSONParam)
    /// <summary>
    /// The ID of the model to use for this request
    /// </summary>
    function Model(const Value: string): TEmbeddingParams;
    /// <summary>
    /// Input one string to get embedding for encoded as one array of tokens
    /// </summary>
    function Input(const Value: string): TEmbeddingParams; overload;
    /// <summary>
    /// Inputting an array of string to get embeddings for encoded as arrays of tokens
    /// </summary>
    function Input(const Value: TArray<string>): TEmbeddingParams; overload;
    /// <summary>
    /// The format of the output data
    /// </summary>
    function Encoding_format(const Value: string): TEmbeddingParams;
    constructor Create; override;
  end;

  TEmbeddingUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPrompt_tokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotal_tokens: Int64;
  public
    property PromptTokens: Int64 read FPrompt_tokens write FPrompt_tokens;
    property TotalTokens: Int64 read FTotal_tokens write FTotal_tokens;
  end;

  TEmbeddingData = class
  private
    [JsonNameAttribute('index')]
    FIndex: Int64;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('embedding')]
    FEmbedding: TArray<Extended>;
  public
    /// <summary>
    /// The index of the embedding in the list of embeddings.
    /// </summary>
    property Index: Int64 read FIndex write FIndex;
    /// <summary>
    /// The object type, which is always "embedding".
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The embedding vector, which is a list of floats. The length of vector depends on the model as listed in the embedding guide.
    /// </summary>
    property Embedding: TArray<Extended> read FEmbedding write FEmbedding;
  end;

  TEmbeddings = class
  private
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonNameAttribute('data')]
    FData: TArray<TEmbeddingData>;
    [JsonNameAttribute('model')]
    FModel: string;
    [JsonNameAttribute('usage')]
    FUsage: TEmbeddingUsage;
  public
    /// <summary>
    /// The Id of the embedding response
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Object of the response; example: list
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Array of the embedding values
    /// </summary>
    property Data: TArray<TEmbeddingData> read FData write FData;
    /// <summary>
    /// Array of the embedding values
    /// </summary>
    property Usage: TEmbeddingUsage read FUsage write FUsage;
    /// <summary>
    /// Tokens used when generating embedding values
    /// </summary>
    property Model: string read FModel write FModel;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Access to the embeddings API allows you to embed sentences
  /// </summary>
  TEmbeddingsRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Creates an embedding vector representing the input text.
    /// </summary>
    function Create(ParamProc: TProc<TEmbeddingParams>): TEmbeddings;
  end;

implementation

{ TEmbeddingParams }

constructor TEmbeddingParams.Create;
begin
  inherited;
  Model('mistral-embed');
  encoding_format('integer');
end;

function TEmbeddingParams.Encoding_format(
  const Value: string): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('encoding_format', Value));
end;

function TEmbeddingParams.Input(const Value: string): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('input', Value));
end;

function TEmbeddingParams.Input(const Value: TArray<string>): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('input', Value));
end;

function TEmbeddingParams.Model(const Value: string): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('model', Value));
end;

{ TEmbeddings }

destructor TEmbeddings.Destroy;
begin
  FUsage.Free;
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TEmbeddingsRoute }

function TEmbeddingsRoute.Create(
  ParamProc: TProc<TEmbeddingParams>): TEmbeddings;
begin
  Result := API.Post<TEmbeddings, TEmbeddingParams>('embeddings', ParamProc);
end;

end.
