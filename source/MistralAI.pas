unit MistralAI;

{+-----------------------------------------------------------------------------+
 |     Github repository : https://github.com/MaxiDonkey/DelphiMistralAI       |
 |     Visit the Github repository for the documentation and use examples      |
 +-----------------------------------------------------------------------------+}

interface

uses
  System.SysUtils, System.Classes, MistralAI.API, System.Net.URLClient,
  MistralAI.Chat, MistralAI.Embeddings, MistralAI.Models, MistralAI.Codestral,
  MistralAI.Files, MistralAI.FineTunings, MistralAI.Agents;

type
  /// <summary>
  /// The IMistralAI interface provides access to the various features and routes of the Mistral AI API.
  /// This interface allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This interface should be implemented by any class that wants to provide a structured way of accessing
  /// the Mistral AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  ///
  /// To use this interface, instantiate a class that implements it, set the required properties such as
  /// <see cref="Token"/> and <see cref="BaseURL"/>, and call the relevant methods for the desired operations.
  /// <code>
  ///   var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
  ///   or
  ///   var CodestralAI: IMistralAI := TMistralAI.Create(API_TOKEN, [CodestralSpec]);
  /// </code>
  /// <seealso cref="TMistralAI"/>
  /// </remarks>
  IMistralAI = interface
    ['{CB506753-77B2-4BD6-A2F8-216433D444A8}']
    function GetAPI: TMistralAIAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetAgentRoute: TAgentRoute;
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddings: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetFineTuningRoute: TFineTuningRoute;
    function GetModels: TModelsRoute;

    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TMistralAIAPI for making API calls.
    /// </returns>
    property API: TMistralAIAPI read GetAPI;
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.mistral.ai/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Provides access to agent completion API.
    /// An AI agent is an autonomous system using large language models (LLM) to perform tasks based on high-level instructions.
    /// </summary>
    /// <returns>
    /// An instance of TAgentRoute for agent-related operations.
    /// </returns>
    property Agent: TAgentRoute read GetAgentRoute;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the Codestral Completion API.
    /// Allows for code design or completion using a template configured to follow specific instructions.
    /// </summary>
    /// <returns>
    /// An instance of TCodestralRoute for code completion operations.
    /// </returns>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Enables the embedding of sentences or documents.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddings;
    /// <summary>
    /// Provides access to the file management API.
    /// Files can be uploaded and used with features such as fine-tuning.
    /// </summary>
    /// <returns>
    /// An instance of TFilesRoute for file-related operations.
    /// </returns>
    property &File: TFilesRoute read GetFilesRoute;
    /// <summary>
    /// Provides access to fine-tuning API for user and organization.
    /// Allows managing fine-tuning jobs.
    /// </summary>
    /// <returns>
    /// An instance of TFineTuningRoute for fine-tuning operations.
    /// </returns>
    property FineTuning: TFineTuningRoute read GetFineTuningRoute;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModels;
  end;

  /// <summary>
  /// Specification taken into account
  /// </summary>
  TSpec = (
    /// <summary>
    /// The "codestral" specification is taken into account in the instantiation of the class
    /// </summary>
    CodestralSpec);

  /// <summary>
  /// List of specifications taken into account
  /// </summary>
  TSpecs = set of TSpec;

  TMistralAIFactory = class
    class function CreateInstance(const AToken: string; Specs: TSpecs = []): IMistralAI;
  end;

  /// <summary>
  /// The TMistralAI class provides access to the various features and routes of the Mistral AI API.
  /// This class allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This class should be implemented by any class that wants to provide a structured way of accessing
  /// the Mistral AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  /// <seealso cref="TMistralAI"/>
  /// </remarks>
  TMistralAI = class(TInterfacedObject, IMistralAI)
  strict private
    FSpecs: TSpecs;
    procedure CodestralCheck;

  private
    FAPI: TMistralAIAPI;
    FAgentRoute: TAgentRoute;
    FChatRoute: TChatRoute;
    FCodestralRoute: TCodestralRoute;
    FEmbeddingsRoute: TEmbeddingsRoute;
    FFileRoute: TFilesRoute;
    FFineTuningRoute: TFineTuningRoute;
    FModelsRoute: TModelsRoute;
    function GetAPI: TMistralAIAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetAgentRoute: TAgentRoute;
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddings: TEmbeddingsRoute;
    function GetFilesRoute: TFilesRoute;
    function GetFineTuningRoute: TFineTuningRoute;
    function GetModels: TModelsRoute;
  public
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TMistralAIAPI for making API calls.
    /// </returns>
    property API: TMistralAIAPI read GetAPI;
    /// <summary>
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.mistral.ai/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public
    /// <summary>
    /// Provides access to agent completion API.
    /// An AI agent is an autonomous system using large language models (LLM) to perform tasks based on high-level instructions.
    /// </summary>
    /// <returns>
    /// An instance of TAgentRoute for agent-related operations.
    /// </returns>
    property Agent: TAgentRoute read GetAgentRoute;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the Codestral Completion API.
    /// Allows for code design or completion using a template configured to follow specific instructions.
    /// </summary>
    /// <returns>
    /// An instance of TCodestralRoute for code completion operations.
    /// </returns>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Provides access to the embeddings API.
    /// Enables the embedding of sentences or documents.
    /// </summary>
    /// <returns>
    /// An instance of TEmbeddingsRoute for embedding operations.
    /// </returns>
    property Embeddings: TEmbeddingsRoute read GetEmbeddings;
    /// <summary>
    /// Provides access to the file management API.
    /// Files can be uploaded and used with features such as fine-tuning.
    /// </summary>
    /// <returns>
    /// An instance of TFilesRoute for file-related operations.
    /// </returns>
    property &File: TFilesRoute read GetFilesRoute;
    // <summary>
    /// Provides access to fine-tuning API for user and organization.
    /// Allows managing fine-tuning jobs.
    /// </summary>
    /// <returns>
    /// An instance of TFineTuningRoute for fine-tuning operations.
    /// </returns>
    property FineTuning: TFineTuningRoute read GetFineTuningRoute;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for model-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModels;
  public
    constructor Create; overload;
    constructor Create(const AToken: string; Specs: TSpecs = []); overload;
    destructor Destroy; override;
  end;

implementation

{ TMistralAI }

constructor TMistralAI.Create;
begin
  inherited;
  FAPI := TMistralAIAPI.Create;
end;

procedure TMistralAI.CodestralCheck;
begin
  if not (CodestralSpec in FSpecs) then
    raise Exception.Create(
       'The MistralAI instance cannot manage "Codestral", for this you must indicate '+
       '[CodestralSpec] as a specification when instantiating the TMistralAI type interface:'#13#13+
       '   TMistralAI.Create(''Your key'', [CodestralSpec])');
end;

constructor TMistralAI.Create(const AToken: string; Specs: TSpecs);
begin
  Create;
  Token := AToken;

  {--- Managing specifications for an instance of the class }
  if CodestralSpec in Specs then
    begin
      FSpecs := FSpecs + [CodestralSpec];
      FAPI.BaseUrl := TMistralAIAPI.URL_BASE_CODESTRAL;
    end;
end;

destructor TMistralAI.Destroy;
begin
  FModelsRoute.Free;
  FEmbeddingsRoute.Free;
  FAgentRoute.Free;
  FChatRoute.Free;
  if CodestralSpec in FSpecs then
    FCodestralRoute.Free;
  FFileRoute.Free;
  FFineTuningRoute.Free;
  FAPI.Free;
  inherited;
end;

function TMistralAI.GetAgentRoute: TAgentRoute;
begin
  if not Assigned(FAgentRoute) then
    FAgentRoute := TAgentRoute.CreateRoute(API);
  Result := FAgentRoute;
end;

function TMistralAI.GetAPI: TMistralAIAPI;
begin
  Result := FAPI;
end;

function TMistralAI.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TMistralAI.GetChatRoute: TChatRoute;
begin
  if not Assigned(FChatRoute) then
    FChatRoute := TChatRoute.CreateRoute(API);
  Result := FChatRoute;
end;

function TMistralAI.GetCodestralRoute: TCodestralRoute;
begin
  CodestralCheck;
  if not Assigned(FCodestralRoute) then
    FCodestralRoute := TCodestralRoute.CreateRoute(API);
  Result := FCodestralRoute;
end;

function TMistralAI.GetEmbeddings: TEmbeddingsRoute;
begin
  if not Assigned(FEmbeddingsRoute) then
    FEmbeddingsRoute := TEmbeddingsRoute.CreateRoute(API);
  Result := FEmbeddingsRoute;
end;

function TMistralAI.GetFilesRoute: TFilesRoute;
begin
  if not Assigned(FFileRoute) then
    FFileRoute := TFilesRoute.CreateRoute(API);
  Result := FFileRoute;
end;

function TMistralAI.GetFineTuningRoute: TFineTuningRoute;
begin
  if not Assigned(FFineTuningRoute) then
    FFineTuningRoute := TFineTuningRoute.CreateRoute(API);
  Result := FFineTuningRoute;
end;

function TMistralAI.GetModels: TModelsRoute;
begin
  if not Assigned(FModelsRoute) then
    FModelsRoute := TModelsRoute.CreateRoute(API);
  Result := FModelsRoute;
end;

function TMistralAI.GetToken: string;
begin
  Result := FAPI.Token;
end;

procedure TMistralAI.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TMistralAI.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TMistralAIFactory }

class function TMistralAIFactory.CreateInstance(const AToken: string;
  Specs: TSpecs): IMistralAI;
begin
  Result := TMistralAI.Create(AToken, Specs);
end;

end.
