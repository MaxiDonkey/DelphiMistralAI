unit MistralAI.Embeddings;

{-------------------------------------------------------------------------------

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, REST.Json.Types, MistralAI.API.Params, MistralAI.API,
  MistralAI.Async.Support, MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents the parameters for an embedding request.
  /// </summary>
  /// <remarks>
  /// Use this class to specify the input text and other parameters for generating embeddings.
  /// </remarks>
  TEmbeddingParams = class(TJSONParam)
    /// <summary>
    /// Sets the ID of the model to use for this request.
    /// </summary>
    /// <param name="Value">
    /// The model ID to be used for embedding.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify the model that will generate the embeddings.
    /// </remarks>
    function Model(const Value: string): TEmbeddingParams;

    /// <summary>
    /// Sets the input text for which to generate an embedding.
    /// </summary>
    /// <param name="Value">
    /// The input text as a string.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify a single string input for embedding.
    /// </remarks>
    function Input(const Value: string): TEmbeddingParams; overload;

    /// <summary>
    /// Sets multiple input texts for which to generate embeddings.
    /// </summary>
    /// <param name="Value">
    /// An array of input texts.
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify multiple strings as inputs for embeddings.
    /// </remarks>
    function Input(const Value: TArray<string>): TEmbeddingParams; overload;

    /// <summary>
    /// Sets the format of the output data.
    /// </summary>
    /// <param name="Value">
    /// The desired output format (e.g., 'float').
    /// </param>
    /// <returns>
    /// The current instance of <c>TEmbeddingParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// Use this method to specify the output format of the embeddings.
    /// </remarks>
    function output_dtype(const Value: string): TEmbeddingParams;

    /// <summary>
    /// The dimension of the output embeddings.
    /// </summary>
    /// <param name="Value">
    /// The vector dimension as an integer.
    /// </param>
    /// <remarks>
    /// only available with the codestral-embed model
    /// </remarks>
    function output_dimension(const Value: Integer): TEmbeddingParams;
  end;

  /// <summary>
  /// Represents token usage statistics for an embedding request.
  /// </summary>
  /// <remarks>
  /// Contains information about the number of tokens used in the request.
  /// </remarks>
  TEmbeddingUsage = class
  private
    [JsonNameAttribute('prompt_tokens')]
    FPrompt_tokens: Int64;
    [JsonNameAttribute('total_tokens')]
    FTotal_tokens: Int64;
  public
    /// <summary>
    /// The number of tokens used in the input text.
    /// </summary>
    property PromptTokens: Int64 read FPrompt_tokens write FPrompt_tokens;

    /// <summary>
    /// The total number of tokens consumed during the embedding request.
    /// </summary>
    property TotalTokens: Int64 read FTotal_tokens write FTotal_tokens;
  end;

  /// <summary>
  /// Represents an embedding result for a single input.
  /// </summary>
  /// <remarks>
  /// Contains the embedding vector and associated metadata for a single input text.
  /// </remarks>
  TEmbeddingData = class
  private
    FIndex: Int64;
    FObject: string;
    FEmbedding: TArray<Double>;
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
    /// The embedding vector, which is a list of floats. The length of the vector depends on the model as listed in the embedding guide.
    /// </summary>
    property Embedding: TArray<Double> read FEmbedding write FEmbedding;
  end;

  /// <summary>
  /// Represents the response from an embedding request.
  /// </summary>
  /// <remarks>
  /// Contains the embeddings, model information, and usage statistics returned by the API.
  /// </remarks>
  TEmbeddings = class(TJSONFingerprint)
  private
    FId: string;
    FObject: string;
    FData: TArray<TEmbeddingData>;
    FModel: string;
    FUsage: TEmbeddingUsage;
  public
    /// <summary>
    /// The unique identifier for the embedding response.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// The object type of the response (e.g., 'list').
    /// </summary>
    property &Object: string read FObject write FObject;

    /// <summary>
    /// The list of embedding data for each input.
    /// </summary>
    property Data: TArray<TEmbeddingData> read FData write FData;

    /// <summary>
    /// The token usage statistics for the embedding request.
    /// </summary>
    property Usage: TEmbeddingUsage read FUsage write FUsage;

    /// <summary>
    /// The model used to generate the embeddings.
    /// </summary>
    property Model: string read FModel write FModel;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the asynchronous callback parameters for embedding requests.
  /// </summary>
  /// <remarks>
  /// Used to handle asynchronous responses for embedding operations.
  /// </remarks>
  TAsyncEmbeddings = TAsyncCallback<TEmbeddings>;

  /// <summary>
  /// Defines a promise-style callback wrapper for embedding results.
  /// </summary>
  /// <remarks>
  /// <c>TPromiseEmbeddings</c> is an alias of <c>TPromiseCallBack&lt;TEmbeddings&gt;</c>,
  /// offering a concise way to handle asynchronous embedding operations that
  /// yield a <c>TEmbeddings</c> instance once completed.
  /// </remarks>
  TPromiseEmbeddings = TPromiseCallback<TEmbeddings>;

  /// <summary>
  /// Provides methods for creating embedding requests.
  /// </summary>
  /// <remarks>
  /// Contains synchronous and asynchronous methods to generate embeddings using the API.
  /// <code>
  /// // Example usage:
  /// var
  ///   EmbeddingsRoute: TEmbeddingsRoute;
  ///   EmbeddingsResult: TEmbeddings;
  /// begin
  ///   EmbeddingsRoute := TEmbeddingsRoute.Create;
  ///   EmbeddingsResult := EmbeddingsRoute.Create(
  ///     procedure(Params: TEmbeddingParams)
  ///     begin
  ///       Params.Input('Your input text here');
  ///     end);
  ///   // Use EmbeddingsResult as needed
  /// end;
  /// </code>
  /// </remarks>
  TEmbeddingsRoute = class(TMistralAIAPIRoute)
  public
    /// <summary>
    /// Initiates an embedding request and returns a promise that resolves with the embeddings.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TEmbeddingParams</c>, including input texts and model settings.
    /// </param>
    /// <param name="Callbacks">
    /// An optional factory function that provides a <c>TPromiseEmbeddings</c> to handle
    /// lifecycle Callbacks (OnStart, OnSuccess, OnError) during the asynchronous operation.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TEmbeddings&gt;</c> which will complete with the <c>TEmbeddings</c> result.
    /// </returns>
    /// <remarks>
    /// Use this method to perform non-blocking embedding requests in a promise-style workflow.
    /// </remarks>
    function AsyncAwaitCreate(const ParamProc: TProc<TEmbeddingParams>;
      const Callbacks: TFunc<TPromiseEmbeddings> = nil): TPromise<TEmbeddings>;

    /// <summary>
    /// Initiates an asynchronous embedding request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// Use this method to perform an embedding request without blocking the main thread.
    /// <code>
    ///   // Example usage:
    ///   MistralAI.Embeddings.AsyncCreate(
    ///     procedure(Params: TEmbeddingParams)
    ///     begin
    ///       Params.Input('Your input text here');
    ///     end,
    ///
    ///     function: TAsyncEmbeddings
    ///     begin
    ///       Result.OnSuccess :=
    ///         procedure(Sender: TObject; Embeddings: TEmbeddings)
    ///         begin
    ///           // Handle successful embedding result
    ///         end;
    ///
    ///       Result.OnError :=
    ///         procedure(Sender: TObject; E: Exception)
    ///         begin
    ///           // Handle error
    ///         end;
    ///     end);
    /// </code>
    /// </remarks>
    procedure AsyncCreate(ParamProc: TProc<TEmbeddingParams>;
      const Callbacks: TFunc<TAsyncEmbeddings>);

    /// <summary>
    /// Performs a synchronous embedding request and returns the result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to set up the embedding parameters.
    /// </param>
    /// <returns>
    /// An instance of <c>TEmbeddings</c> containing the embedding results.
    /// </returns>
    /// <remarks>
    /// Use this method to perform an embedding request that blocks until the response is received.
    /// <code>
    ///   // Example usage:
    ///   var Embeddings := MistralAI.Embeddings.Create(
    ///     procedure(Params: TEmbeddingParams)
    ///     begin
    ///       Params.Input(['Your input text here']);
    ///     end);
    ///    try
    ///      // Use Embeddings as needed
    ///    finally
    ///      Embeddings.Free;
    ///    end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TEmbeddingParams>): TEmbeddings;
  end;

implementation

{ TEmbeddingParams }

function TEmbeddingParams.output_dimension(
  const Value: Integer): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('output_dimension', Value));
end;

function TEmbeddingParams.output_dtype(
  const Value: string): TEmbeddingParams;
begin
  Result := TEmbeddingParams(Add('output_dtype', Value));
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

function TEmbeddingsRoute.AsyncAwaitCreate(
  const ParamProc: TProc<TEmbeddingParams>;
  const Callbacks: TFunc<TPromiseEmbeddings>): TPromise<TEmbeddings>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEmbeddings>(
    procedure(const CallbackParams: TFunc<TAsyncEmbeddings>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TEmbeddingsRoute.AsyncCreate(ParamProc: TProc<TEmbeddingParams>;
  const Callbacks: TFunc<TAsyncEmbeddings>);
begin
  with TAsyncCallBackExec<TAsyncEmbeddings, TEmbeddings>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEmbeddings
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

function TEmbeddingsRoute.Create(
  ParamProc: TProc<TEmbeddingParams>): TEmbeddings;
begin
  Result := API.Post<TEmbeddings, TEmbeddingParams>('embeddings', ParamProc);
end;

end.
