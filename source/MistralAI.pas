unit MistralAI;

interface

uses
  System.SysUtils, System.Classes, MistralAI.API, System.Net.URLClient,
  MistralAI.Chat, MistralAI.Embeddings, MistralAI.Models, MistralAI.Codestral;

type
  IMistralAI = interface
    ['{CB506753-77B2-4BD6-A2F8-216433D444A8}']
    function GetAPI: TMistralAIAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddings: TEmbeddingsRoute;
    function GetModels: TModelsRoute;

    /// <summary>
    /// Direct access to queries
    /// </summary>
    property API: TMistralAIAPI read GetAPI;
    /// <summary>
    /// The OpenAI API uses API keys for authentication.
    /// Visit your API Keys page (https://console.mistral.ai/user/api-keys) to retrieve the API key you'll use in your requests.
    /// Remember that your API key is a secret! Do not share it with others or expose it in any client-side code (browsers, apps).
    /// Production requests must be routed through your own backend server where your API key can be securely
    /// loaded from an environment variable or key management service.
    /// </summary>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Base Url (https://api.mistral.ai/v1) by default
    /// If the "CodestralSpec" flag is set to the "Specs" value, then the base URL is thus modified (https://codestral.mistral.ai/v1)
    /// </summary>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Access to the chat completion API allows you to chat with a model fine-tuned to follow instructions.
    /// </summary>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Access to the Codestral Completion API allows you to design or complete code with a template configured to follow instructions.
    /// </summary>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Access to the embeddings API allows you to embed sentences
    /// </summary>
    property Embeddings: TEmbeddingsRoute read GetEmbeddings;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
    property Models: TModelsRoute read GetModels;
  end;

  TSpec = (
    /// <summary>
    /// The "codestral" specification is taken into account in the instantiation of the class
    /// </summary>
    CodestralSpec);

  /// <summary>
  /// List of specifications taken into account
  /// </summary>
  TSpecs = set of TSpec;

  TMistralAI = class(TInterfacedObject, IMistralAI)
  strict private
    FSpecs: TSpecs;
    procedure CodestralCheck;

  private
    FAPI: TMistralAIAPI;
    FChatRoute: TChatRoute;
    FCodestralRoute: TCodestralRoute;
    FEmbeddings: TEmbeddingsRoute;
    FModels: TModelsRoute;
    function GetAPI: TMistralAIAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetCodestralRoute: TCodestralRoute;
    function GetEmbeddings: TEmbeddingsRoute;
    function GetModels: TModelsRoute;

  public
    /// <summary>
    /// Direct access to API
    /// </summary>
    property API: TMistralAIAPI read GetAPI;
    /// <summary>
    /// The OpenAI API uses API keys for authentication.
    /// Visit your API Keys page (https://console.mistral.ai/user/api-keys) to retrieve the API key you'll use in your requests.
    /// Remember that your API key is a secret! Do not share it with others or expose it in any client-side code (browsers, apps).
    /// Production requests must be routed through your own backend server where your API key can be securely
    /// loaded from an environment variable or key management service.
    /// </summary>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Base Url (https://api.mistral.ai/v1)
    /// </summary>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public
    /// <summary>
    /// Access to the chat completion API allows you to chat with a model fine-tuned to follow instructions.
    /// </summary>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Access to the Codestral Completion API allows you to design or complete code with a template configured to follow instructions.
    /// </summary>
    property Codestral: TCodestralRoute read GetCodestralRoute;
    /// <summary>
    /// Access to the embeddings API allows you to embed sentences
    /// </summary>
    property Embeddings: TEmbeddingsRoute read GetEmbeddings;
    /// <summary>
    /// Lists and describes the various models available in the API.
    /// You can refer to the Models documentation to understand what models are available and the differences between them.
    /// </summary>
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
  FModels.Free;
  FEmbeddings.Free;
  FChatRoute.Free;
  if CodestralSpec in FSpecs then
    FCodestralRoute.Free;
  FAPI.Free;
  inherited;
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
  if not Assigned(FEmbeddings) then
    FEmbeddings := TEmbeddingsRoute.CreateRoute(API);
  Result := FEmbeddings;
end;

function TMistralAI.GetModels: TModelsRoute;
begin
  if not Assigned(FModels) then
    FModels := TModelsRoute.CreateRoute(API);
  Result := FModels;
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

end.
