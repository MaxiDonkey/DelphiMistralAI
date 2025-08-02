unit MistralAI.API;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

{$REGION 'Dev note'}
  (*
    --- NOTE ---
    The MistralAI.HttpClientInterface unit defines  an IHttpClientAPI  interface, which
    allows  for decoupling  the specific implementation  of  the HTTP  client used  for
    web requests. This introduces  an abstraction  that  enhances flexibility, improves
    testability, and simplifies code maintenance.

    The IHttpClientAPI interface  ensures that  client code can interact  with  the web
    without  being  dependent  on a specific class, thus  facilitating  the replacement
    or modification  of the  underlying  HTTP implementation  details without impacting
    the rest  of  the application. It also  enables  easy mocking  during unit testing,
    offering the ability to test  HTTP request behaviors in an isolated  and controlled
    manner.

    This approach adheres to the SOLID principles of dependency inversion, contributing
    to a robust, modular, and adaptable software architecture.
  *)
{$ENDREGION}

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.URLClient,
  System.Net.Mime, System.JSON, System.Generics.Collections,
  MistralAI.API.Params, MistralAI.Errors, MistralAI.Exception, MistralAI.Monitoring,
  MistralAI.HttpClientInterface, MistralAI.HttpClientAPI, MistralAI.API.Helper,
  MistralAI.API.Normalizer;

type
  /// <summary>
  /// Represents the configuration settings for the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class provides properties and methods to manage the API key, base URL,
  /// organization identifier, and custom headers for communicating with the MistralAI API.
  /// It also includes utility methods for building headers and endpoint URLs.
  /// </remarks>
  TMistralAISettings = class
  const
    URL_BASE = 'https://api.mistral.ai/v1';
    URL_BASE_CODESTRAL = 'https://codestral.mistral.ai/v1';
  private
    FAPIKey: string;
    FBaseUrl: string;
    FOrganization: string;
    FCustomHeaders: TNetHeaders;
    procedure SetAPIKey(const Value: string);
    procedure SetBaseUrl(const Value: string);
    procedure SetOrganization(const Value: string);
    procedure SetCustomHeaders(const Value: TNetHeaders);
    procedure VerifyApiSettings;
    procedure ResetCustomHeader;
  protected
    /// <summary>
    /// Retrieves the headers required for API requests.
    /// </summary>
    /// <returns>
    /// A list of headers including authorization and optional organization information.
    /// </returns>
    function BuildHeaders: TNetHeaders; virtual;

    /// <summary>
    /// Builds headers specific to JSON-based API requests.
    /// </summary>
    /// <returns>
    /// A list of headers including JSON content-type and authorization details.
    /// </returns>
    function BuildJsonHeaders: TNetHeaders; virtual;

    /// <summary>
    /// Constructs the full URL for a specific API endpoint.
    /// </summary>
    /// <param name="Endpoint">
    /// The relative endpoint path (e.g. "models").
    /// </param>
    /// <returns>
    /// The full URL including the base URL and endpoint.
    /// </returns>
    function BuildUrl(const Endpoint: string): string; overload; virtual;

    /// <summary>
    /// Constructs the full URL for a specific API endpoint.
    /// </summary>
    /// <param name="Endpoint">
    /// The relative endpoint path (e.g. "models").
    /// </param>
    /// <param name="Parameters">
    /// e.g. "?param1=val1&param2=val2...."
    /// </param>
    /// <returns>
    /// The full URL including the base URL and endpoint.
    /// </returns>
    function BuildUrl(const Endpoint, Parameters: string): string; overload; virtual;
  public
    constructor Create; overload;

    /// <summary>
    /// The API key used for authentication.
    /// </summary>
    property APIKey: string read FAPIKey write SetAPIKey;

    /// <summary>
    /// The base URL for API requests.
    /// </summary>
    property BaseUrl: string read FBaseUrl write SetBaseUrl;

    /// <summary>
    /// The organization identifier used for the API.
    /// </summary>
    property Organization: string read FOrganization write SetOrganization;

    /// <summary>
    /// Custom headers to include in API requests.
    /// </summary>
    property CustomHeaders: TNetHeaders read FCustomHeaders write SetCustomHeaders;
  end;

  /// <summary>
  /// Handles HTTP requests and responses for the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class extends <c>TMistralAISettings</c> and provides a mechanism to
  /// manage HTTP client interactions for the API, including configuration and request execution.
  /// </remarks>
  TApiHttpClient = class(TMistralAISettings)
  private
    /// <summary>
    /// The HTTP client interface used for making API calls.
    /// </summary>
    FHTTPClient: IHttpClientAPI;
  public
    constructor Create;

    /// <summary>
    /// The HTTP client used to send requests to the API.
    /// </summary>
    /// <value>
    /// An instance of a class implementing <c>IHttpClientAPI</c>.
    /// </value>
    property HttpClient: IHttpClientAPI read FHTTPClient;
  end;

  /// <summary>
  /// Manages and processes errors from the MistralAI API responses.
  /// </summary>
  /// <remarks>
  /// This class extends <c>TApiHttpClient</c> and provides error-handling capabilities
  /// by parsing error data and raising appropriate exceptions.
  /// </remarks>
  TApiDeserializer = class(TApiHttpClient)
  class var Metadata: ICustomFieldsPrepare;
  protected
    /// <summary>
    /// Parses the error data from the API response.
    /// </summary>
    /// <param name="Code">
    /// The HTTP status code returned by the API.
    /// </param>
    /// <param name="ResponseText">
    /// The response body containing error details.
    /// </param>
    /// <exception cref="MistralAIException">
    /// Raised if the error response cannot be parsed or contains invalid data.
    /// </exception>
    procedure DeserializeErrorData(const Code: Int64; const ResponseText: string); virtual;

    /// <summary>
    /// Raises an exception corresponding to the API error code.
    /// </summary>
    /// <param name="Code">
    /// The HTTP status code returned by the API.
    /// </param>
    /// <param name="Error">
    /// The deserialized error object containing error details.
    /// </param>
    procedure RaiseError(Code: Int64; Error: TErrorCore); virtual;

    /// <summary>
    /// Deserializes the API response into a strongly typed object.
    /// </summary>
    /// <param name="T">
    /// The type of the object to deserialize into. It must be a class with a parameterless constructor.
    /// </param>
    /// <param name="Code">
    /// The HTTP status code of the API response.
    /// </param>
    /// <param name="ResponseText">
    /// The response body as a JSON string.
    /// </param>
    /// <returns>
    /// A deserialized object of type <c>T</c>.
    /// </returns>
    /// <exception cref="MistralAIExceptionInvalidResponse">
    /// Raised if the response is non-compliant or deserialization fails.
    /// </exception>
    function Deserialize<T: class, constructor>(const Code: Int64; const ResponseText: string): T;
  public
    class constructor Create;

    /// <summary>
    /// Deserializes the API response into a strongly typed object.
    /// </summary>
    /// <param name="T">
    /// The type of the object to deserialize into. It must be a class with a parameterless constructor.
    /// </param>
    /// <param name="ResponseText">
    /// The response body as a JSON string.
    /// </param>
    /// <returns>
    /// A deserialized object of type <c>T</c>.
    /// </returns>
    /// <exception cref="MistralAIExceptionInvalidResponse">
    /// Raised if the response is non-compliant or deserialization fails.
    /// </exception>
    class function Parse<T: class, constructor>(const Value: string): T;
  end;

  /// <summary>
  /// Provides a high-level interface for interacting with the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class extends <c>TApiDeserializer</c> and includes methods for making HTTP requests to
  /// the MistralAI API. It supports various HTTP methods, including GET, POST, PATCH, and DELETE,
  /// as well as handling file uploads and downloads. The API key and other configuration settings
  /// are inherited from the <c>TMistralAISettings</c> class.
  /// </remarks>
  TMistralAIAPI = class(TApiDeserializer)
  public
    function Get<TResult: class, constructor>(const Endpoint: string; const Path: TArray<string> = []): TResult; overload;
    function Get<TResult: class, constructor>(const Endpoint: string; const ListFieldName: string): TResult; overload;
    function Get<TResult: class, constructor; TParams: TUrlParam>(const Endpoint: string; ParamProc: TProc<TParams>; const ListFieldName: string = ''): TResult; overload;
    function GetEx<TResult: class, constructor>(const Endpoint: string): TResult; overload;
    function GetFile(const Endpoint: string; Response: TStream): Integer; overload;
    function GetFile<TResult: class, constructor>(const Endpoint: string; const JSONFieldName: string = 'data'): TResult; overload;
    function Delete<TResult: class, constructor>(const Endpoint: string): TResult; overload;
    function Delete<TResult: class, constructor; TParams: TJSONParam>(const Endpoint: string; ParamProc: TProc<TParams>): TResult; overload;
    function DeleteEx<TResult: class, constructor>(const Endpoint: string): TResult; overload;
    function Post(const Endpoint: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;
    function Post<TParams: TJSONParam>(const Endpoint: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean; overload;
    function Post<TResult: class, constructor; TParams: TJSONParam>(const Endpoint: string; ParamProc: TProc<TParams>; const Path: TArray<string> = []): TResult; overload;
    function Post<TResult: class, constructor>(const Endpoint: string): TResult; overload;
    function PostEx<TResult: class, constructor>(const Endpoint: string): TResult; overload;
    function Patch<TResult: class, constructor; TParams: TJSONParam>(const Endpoint: string; ParamProc: TProc<TParams>): TResult;
    function PatchFromUrl<TResult: class, constructor; TParams: TUrlParam>(const Endpoint: string; ParamProc: TProc<TParams>): TResult;
    function PostForm<TResult: class, constructor; TParams: TMultipartFormData, constructor>(const Endpoint: string; ParamProc: TProc<TParams>): TResult;
    function Put<TResult: class, constructor; TParams: TJSONParam>(const Endpoint: string; ParamProc: TProc<TParams>): TResult;

    /// <summary>
    /// Initializes a new instance of the <c>TMistralAIAPI</c> class with an API key.
    /// </summary>
    /// <param name="AAPIKey">
    /// The API key used for authenticating requests to the MistralAI API.
    /// </param>
    constructor Create(const AAPIKey: string); overload;
  end;

  /// <summary>
  /// Represents a specific route or logical grouping for the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class allows associating a <c>TMistralAIAPI</c> instance with specific routes or
  /// endpoints, providing an organized way to manage API functionality.
  /// </remarks>
  TMistralAIAPIRoute = class
  private
    /// <summary>
    /// The MistralAI API instance associated with this route.
    /// </summary>
    FAPI: TMistralAIAPI;
    procedure SetAPI(const Value: TMistralAIAPI);
  protected
    procedure HeaderCustomize; virtual;
  public
    /// <summary>
    /// The MistralAI API instance associated with this route.
    /// </summary>
    property API: TMistralAIAPI read FAPI write SetAPI;

    /// <summary>
    /// Initializes a new instance of the <c>TMistralAIRoute</c> class with the given API instance.
    /// </summary>
    /// <param name="AAPI">
    /// The <c>TMistralAIAPI</c> instance to associate with the route.
    /// </param>
    constructor CreateRoute(AAPI: TMistralAIAPI); reintroduce;
  end;

var
  MetadataAsObject: Boolean = False;

implementation

uses
  System.StrUtils, REST.Json, MistralAI.NetEncoding.Base64;

constructor TMistralAIAPI.Create(const AAPIKey: string);
begin
  Create;
  APIKey := AAPIKey;
end;

function TMistralAIAPI.Delete<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HTTPClient.Delete(BuildUrl(Endpoint), Params.JSON, Response, BuildJsonHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Delete<TResult>(const Endpoint: string): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Delete(BuildUrl(Endpoint), Response, BuildHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.DeleteEx<TResult>(const Endpoint: string): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Delete(BuildUrl(Endpoint), Response, BuildHeaders);
    var S := Response.DataString;
    if S.IsEmpty then
      S := '{"processed":true}';
    Result := Deserialize<TResult>(Code, S);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Get<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>; const ListFieldName: string): TResult;
begin
  Monitoring.Inc;
  Result := nil;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HTTPClient.Get(BuildUrl(Endpoint, Params.Value), Response, BuildHeaders);
    case Code of
      200..299:
        if ListFieldName.IsEmpty then
          Result := Deserialize<TResult>(Code, Response.DataString)
        else
          Result := Deserialize<TResult>(Code, format('{"%s":%s}', [ListFieldName, Response.DataString]));
      else
        RaiseError(Code, nil);
    end;
  finally
    Response.Free;
    Params.Free;
    ResetCustomHeader;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Get<TResult>(const Endpoint,
  ListFieldName: string): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Get(BuildUrl(Endpoint), Response, BuildHeaders);
    if ListFieldName.IsEmpty then
      Result := Deserialize<TResult>(Code, Response.DataString)
    else
      Result := Deserialize<TResult>(Code, format('{"%s":%s}', [ListFieldName, Response.DataString]));
  finally
    Response.Free;
    ResetCustomHeader;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Get<TResult>(const Endpoint: string;
  const Path: TArray<string>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Get(BuildUrl(Endpoint), Response, BuildHeaders);
    if Length(Path) = 0 then
      Result := Deserialize<TResult>(Code, Response.DataString)
    else
      Result := Deserialize<TResult>(Code, TJSONNormalizer.Normalize(Response.DataString, Path));
  finally
    Response.Free;
    ResetCustomHeader;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.GetEx<TResult>(const Endpoint: string): TResult;
begin
  Monitoring.Inc;
  Result := nil;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Get(BuildUrl(Endpoint), Response, BuildHeaders);
    case Code of
      200..299:
        try
          begin
            Result := TJson.JsonToObject<TResult>(Response.DataString);
            if Assigned(Result) and TResult.InheritsFrom(TJSONFingerprint) then
              begin
                var JSONValue := TJSONObject.ParseJSONValue(Response.DataString);
                try
                  (Result as TJSONFingerprint).JSONResponse := JSONValue.Format();
                finally
                  JSONValue.Free;
                end;
              end;
          end;
        except
          Result := nil;
        end;
      else
        DeserializeErrorData(Code, Response.DataString);
    end;
  finally
    Response.Free;
    ResetCustomHeader;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.GetFile<TResult>(const Endpoint: string; const JSONFieldName: string):TResult;
begin
  var Stream := TStringStream.Create;
  try
    var Code := GetFile(Endpoint, Stream);
    Stream.Position := 0;
    var Temp := TStringStream.Create(BytesToString(Stream.Bytes).TrimRight([#0]));
    try
      Result := Deserialize<TResult>(Code, Format('{"%s":"%s"}', [JSONFieldName, EncodeBase64(Temp)]));
    finally
      Temp.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TMistralAIAPI.GetFile(const Endpoint: string; Response: TStream): Integer;
begin
  Monitoring.Inc;
  try
    Result := FHTTPClient.Get(BuildUrl(Endpoint), Response, BuildHeaders);
    case Result of
      200..299:
        {success};
      else
        begin
          var Recieved := TStringStream.Create;
          try
            Response.Position := 0;
            Recieved.LoadFromStream(Response);
            DeserializeErrorData(Result, Recieved.DataString);
          finally
            Recieved.Free;
          end;
        end;
    end;
  finally
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Patch<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HTTPClient.Patch(BuildUrl(Endpoint), Params.JSON, Response, BuildJsonHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.PatchFromUrl<TResult, TParams>(
  const Endpoint: string; ParamProc: TProc<TParams>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HTTPClient.Patch(BuildUrl(Endpoint, Params.Value), nil, Response, BuildJsonHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Post(const Endpoint: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback): Integer;
begin
  Monitoring.Inc;
  try
    Result := HTTPClient.Post(BuildUrl(Endpoint), Body, Response, BuildJsonHeaders, OnReceiveData);
  finally
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Post<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>;
  const Path: TArray<string>): TResult;
begin
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := Post(Endpoint, Params.JSON, Response);
    if Length(Path) = 0 then
      Result := Deserialize<TResult>(Code, Response.DataString)
    else
      Result := Deserialize<TResult>(Code, TJSONNormalizer.Normalize(Response.DataString, Path));
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TMistralAIAPI.Post<TParams>(const Endpoint: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean;
begin
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := Post(Endpoint, Params.JSON, Response, Event);
    case Code of
      200..299:
        Result := True;
    else
      begin
        Result := False;
        var Recieved := TStringStream.Create;
        try
          Response.Position := 0;
          Recieved.LoadFromStream(Response);
          DeserializeErrorData(Code, Recieved.DataString);
        finally
          Recieved.Free;
        end;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TMistralAIAPI.Post<TResult>(const Endpoint: string): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Post(BuildUrl(Endpoint), Response, BuildHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.PostEx<TResult>(const Endpoint: string): TResult;
begin
  Monitoring.Inc;
  Result := nil;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  try
    var Code := HTTPClient.Post(BuildUrl(Endpoint), Response, BuildHeaders);
    case Code of
      200..299:
        try
          begin
            var S := Response.DataString;
            if S.IsEmpty then
              S := '{"processed":true}';
            Result := Deserialize<TResult>(Code, S);
          end;
        except
          Result := nil;
        end;
      else
        DeserializeErrorData(Code, Response.DataString);
    end;
  finally
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.PostForm<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HTTPClient.Post(BuildUrl(Endpoint), Params, Response, BuildHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

function TMistralAIAPI.Put<TResult, TParams>(const Endpoint: string;
  ParamProc: TProc<TParams>): TResult;
begin
  Monitoring.Inc;
  var Response := TStringStream.Create('', TEncoding.UTF8);
  var Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    var Code := HttpClient.Put(BuildUrl(Endpoint), Params.JSON, Response, BuildJsonHeaders);
    Result := Deserialize<TResult>(Code, Response.DataString)
  finally
    Params.Free;
    Response.Free;
    Monitoring.Dec;
  end;
end;

{ TMistralAIAPIRoute }

constructor TMistralAIAPIRoute.CreateRoute(AAPI: TMistralAIAPI);
begin
  inherited Create;
  FAPI := AAPI;
end;

procedure TMistralAIAPIRoute.HeaderCustomize;
begin

end;

procedure TMistralAIAPIRoute.SetAPI(const Value: TMistralAIAPI);
begin
  FAPI := Value;
end;

{ TMistralAISettings }

function TMistralAISettings.BuildHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('Authorization', 'Bearer ' + FAPIKey)] + FCustomHeaders;
end;

function TMistralAISettings.BuildUrl(const Endpoint: string): string;
begin
  Result := FBaseUrl.TrimRight(['/']) + '/' + Endpoint.TrimLeft(['/']);
end;

function TMistralAISettings.BuildJsonHeaders: TNetHeaders;
begin
  Result := BuildHeaders +
    [TNetHeader.Create('Content-Type', 'application/json')] +
    [TNetHeader.Create('Accept', 'application/json')];
end;

function TMistralAISettings.BuildUrl(const Endpoint,
  Parameters: string): string;
begin
  Result := BuildUrl(EndPoint) + Parameters;
end;

constructor TMistralAISettings.Create;
begin
  inherited;
  FAPIKey := EmptyStr;
  FBaseUrl := URL_BASE;
end;

procedure TMistralAISettings.ResetCustomHeader;
begin
  CustomHeaders := [];
end;

procedure TMistralAISettings.SetAPIKey(const Value: string);
begin
  FAPIKey := Value;
end;

procedure TMistralAISettings.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

procedure TMistralAISettings.SetCustomHeaders(const Value: TNetHeaders);
begin
  FCustomHeaders := Value;
end;

procedure TMistralAISettings.SetOrganization(const Value: string);
begin
  FOrganization := Value;
end;

procedure TMistralAISettings.VerifyApiSettings;
begin
  if FAPIKey.IsEmpty then
    raise MistralAIExceptionAPI.Create('Token is empty!');
  if FBaseUrl.IsEmpty then
    raise MistralAIExceptionAPI.Create('Base url is empty!');
end;

{ TApiHttpClient }

constructor TApiHttpClient.Create;
begin
  inherited Create;
  FHttpClient := THttpClientAPI.CreateInstance(VerifyApiSettings);
end;

{ TApiDeserializer }

class constructor TApiDeserializer.Create;
begin
  Metadata := TDeserializationPrepare.CreateInstance;
end;

function TApiDeserializer.Deserialize<T>(const Code: Int64;
  const ResponseText: string): T;
begin
  Result := nil;
  case Code of
    200..299:
      try
        Result := Parse<T>(ResponseText);
      except
        Result := nil;
      end;
    else
      DeserializeErrorData(Code, ResponseText);
  end;
  if not Assigned(Result) then
    raise EInvalidResponse.Create(Code, 'Non-compliant response');
end;

procedure TApiDeserializer.DeserializeErrorData(const Code: Int64;
  const ResponseText: string);
var
  Error: TErrorCore;
begin
  Error := nil;
  try
    try
      case Code of
        422 : Error := TJson.JsonToObject<TError422>(ResponseText);
        else
          Error := TJson.JsonToObject<TError>(ResponseText);
      end;
    except
      Error := nil;
    end;
    if Assigned(Error) then
      RaiseError(Code, Error);
  finally
    if Assigned(Error) then
      Error.Free;
  end;
end;

class function TApiDeserializer.Parse<T>(const Value: string): T;
begin
  {$REGION 'Dev note'}
     (*
      - If Metadata are to be treated  as objects, a dedicated  TMetadata class is required, containing
      all the properties corresponding to the specified JSON fields.

      - However, if Metadata are  not treated  as objects, they will be temporarily handled as a string
      and subsequently converted back into a valid JSON string during the deserialization process using
      the revert method of the interceptor.

      By default, Metadata are  treated as strings rather  than objects to handle  cases where multiple
      classes to be deserialized may contain variable data structures.
      Refer to the global variable MetadataAsObject.
     *)
  {$ENDREGION}
  case MetadataAsObject of
    True:
      Result := TJson.JsonToObject<T>(Value);
    else
      Result := TJson.JsonToObject<T>(Metadata.Convert(Value));
  end;

  {--- Add JSON response if class inherits from TJSONFingerprint class. }
  if Assigned(Result) and T.InheritsFrom(TJSONFingerprint) then
    begin
      var JSONValue := TJSONObject.ParseJSONValue(Value);
      try
        (Result as TJSONFingerprint).JSONResponse := JSONValue.Format();
      finally
        JSONValue.Free;
      end;
    end;
end;

procedure TApiDeserializer.RaiseError(Code: Int64; Error: TErrorCore);
begin
  case Code of
    429:
      raise ERateLimitError.Create(Code, Error);
    400, 404, 415:
      raise EInvalidRequestError.Create(Code, Error);
    401:
      raise EAuthenticationError.Create(Code, Error);
    403:
      raise EPermissionError.Create(Code, Error);
    409:
      raise ETryAgain.Create(Code, Error);
    422:
      raise EUnprocessableEntityError.Create(Code, Error);
    500:
      raise EEngineException.Create(Code, Error);
  else
    raise MistralAIException.Create(Code, Error);
  end;
end;

end.

