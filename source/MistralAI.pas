unit MistralAI;

interface

uses
  System.SysUtils, System.Classes, MistralAI.API, System.Net.URLClient,
  MistralAI.Chat, MistralAI.Embeddings, MistralAI.Models;

type
  IMistralAI = interface
    ['{CB506753-77B2-4BD6-A2F8-216433D444A8}']
    function GetAPI: TMistralAIAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
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
    /// Base Url (https://api.mistral.ai/v1)
    /// </summary>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

    /// <summary>
    /// Access to the chat completion API allows you to chat with a model fine-tuned to follow instructions.
    /// </summary>
    property Chat: TChatRoute read GetChatRoute;
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

  TMistralAI = class(TInterfacedObject, IMistralAI)
  private
    FAPI: TMistralAIAPI;
    FChatRoute: TChatRoute;
    FEmbeddings: TEmbeddingsRoute;
    FModels: TModelsRoute;
    function GetAPI: TMistralAIAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
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
    constructor Create(const AToken: string); overload;
    destructor Destroy; override;
  end;

implementation

{ TMistralAI }

constructor TMistralAI.Create;
begin
  inherited;
  FAPI := TMistralAIAPI.Create;
end;

constructor TMistralAI.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TMistralAI.Destroy;
begin
  FModels.Free;
  FEmbeddings.Free;
  FChatRoute.Free;
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
