unit MistralAI.API;

interface

uses
  System.Classes, System.Net.HttpClient, System.Net.URLClient, System.Net.Mime,
  System.JSON, MistralAI.API.Params, MistralAI.Errors, System.SysUtils;

type
  MistralAIException = class(Exception)
  private
    FCode: Int64;
    FMsg: string;
  public
    constructor Create(const ACode: Int64; const AError: TErrorCore); reintroduce; overload;
    constructor Create(const ACode: Int64; const Value: string); reintroduce; overload;
    property Code: Int64 read FCode write FCode;
    property Msg: string read FMsg write FMsg;
  end;

  MistralAIValidationException = class(Exception)
  private
    FCode: Int64;
  protected
    function DetailItemToStr(const Value: TDetail): string;
    function LocToStr(const Value: TArray<string>): string;
    function ToText(const AError: TError422): string;
    function TypeToStr(const Value: string): string;
  public
    constructor Create(const ACode: Int64; const AError: TErrorCore); reintroduce;
    property Code: Int64 read FCode write FCode;
  end;

  /// <summary>
  /// The `MistralAIExceptionAPI` class represents a generic API-related exception.
  /// It is thrown when there is an issue with the API configuration or request process,
  /// such as a missing API token, invalid base URL, or other configuration errors.
  /// This class serves as a base for more specific API exceptions.
  /// </summary>
  MistralAIExceptionAPI = class(Exception);

  /// <summary>
  /// An InvalidRequestError indicates that your request was malformed or
  /// missing some required parameters, such as a token or an input.
  /// This could be due to a typo, a formatting error, or a logic error in your code.
  /// </summary>
  MistralAIExceptionInvalidRequestError = class(MistralAIException);

  /// <summary>
  /// A `RateLimitError` indicates that you have hit your assigned rate limit.
  /// This means that you have sent too many tokens or requests in a given period of time,
  /// and our services have temporarily blocked you from sending more.
  /// </summary>
  MistralAIExceptionRateLimitError = class(MistralAIException);

  /// <summary>
  /// An `AuthenticationError` indicates that your API key or token was invalid,
  /// expired, or revoked. This could be due to a typo, a formatting error, or a security breach.
  /// </summary>
  MistralAIExceptionAuthenticationError = class(MistralAIException);

  /// <summary>
  /// This error message indicates that your account is not part of an organization
  /// </summary>
  MistralAIExceptionPermissionError = class(MistralAIException);

  /// <summary>
  /// This error message indicates that our servers are experiencing high
  /// traffic and are unable to process your request at the moment
  /// </summary>
  MistralAIExceptionTryAgain = class(MistralAIException);

  /// <summary>
  /// This error occurs when a request to the API can not be processed. This is a client-side error,
  /// meaning the problem is with the request itself, and not the API.
  /// </summary>
  MistralUnprocessableEntityError = class(MistralAIValidationException);

  /// <summary>
  /// An `InvalidResponse` error occurs when the API response is either empty or not in the expected format.
  /// This error indicates that the API did not return a valid response that can be processed, possibly due to a server-side issue,
  /// a malformed request, or unexpected input data.
  /// </summary>
  MistralAIExceptionInvalidResponse = class(MistralAIException);

  TMistralAIAPI = class
  public
    const
      URL_BASE = 'https://api.mistral.ai/v1';
      URL_BASE_CODESTRAL = 'https://codestral.mistral.ai/v1';
  private
    FHTTPClient: THTTPClient;
    FToken: string;
    FBaseUrl: string;
    FOrganization: string;
    FCustomHeaders: TNetHeaders;
    procedure SetToken(const Value: string);
    procedure SetBaseUrl(const Value: string);
    procedure SetOrganization(const Value: string);
    procedure RaiseError(Code: Int64; Error: TErrorCore);
    procedure ParseError(const Code: Int64; const ResponseText: string);
    procedure SetCustomHeaders(const Value: TNetHeaders);

  protected
    function GetHeaders: TNetHeaders;
    function GetRequestURL(const Path: string): string;
    function Get(const Path: string; Response: TStringStream): Integer; overload;
    function Delete(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;
    function Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer; overload;
    function ParseResponse<T: class, constructor>(const Code: Int64; const ResponseText: string): T;
    procedure CheckAPI;

  public
    function Get<TResult: class, constructor>(const Path: string): TResult; overload;
    procedure GetFile(const Path: string; Response: TStream); overload;
    function Delete<TResult: class, constructor>(const Path: string): TResult; overload;
    function Post<TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean; overload;
    function Post<TResult: class, constructor; TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;
    function Post<TResult: class, constructor>(const Path: string): TResult; overload;
    function PostForm<TResult: class, constructor; TParams: TMultipartFormData, constructor>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;

  public
    constructor Create; overload;
    constructor Create(const AToken: string); overload;
    destructor Destroy; override;
    property Token: string read FToken write SetToken;
    property BaseUrl: string read FBaseUrl write SetBaseUrl;
    property Organization: string read FOrganization write SetOrganization;
    property Client: THTTPClient read FHTTPClient;
    property CustomHeaders: TNetHeaders read FCustomHeaders write SetCustomHeaders;
  end;

  TMistralAIAPIRoute = class
  private
    FAPI: TMistralAIAPI;
    procedure SetAPI(const Value: TMistralAIAPI);
  public
    property API: TMistralAIAPI read FAPI write SetAPI;
    constructor CreateRoute(AAPI: TMistralAIAPI); reintroduce;
  end;

implementation

uses
  System.StrUtils, REST.Json;

constructor TMistralAIAPI.Create;
begin
  inherited;
  FHTTPClient := THTTPClient.Create;
  FToken := EmptyStr;
  FBaseUrl := URL_BASE;
end;

constructor TMistralAIAPI.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TMistralAIAPI.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TMistralAIAPI.Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback): Integer;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
begin
  CheckAPI;
  Headers := GetHeaders + [TNetHeader.Create('Content-Type', 'application/json')];
  Headers := Headers + [TNetHeader.Create('Accept', 'application/json')];
  Stream := TStringStream.Create;
  FHTTPClient.ReceiveDataCallBack := OnReceiveData;
  try
    Stream.WriteString(Body.ToJSON);
    Stream.Position := 0;
    Result := FHTTPClient.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
  finally
    FHTTPClient.OnReceiveData := nil;
    Stream.Free;
  end;
end;

function TMistralAIAPI.Get(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Get(GetRequestURL(Path), Response, Headers).StatusCode;
end;

function TMistralAIAPI.Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Post(GetRequestURL(Path), Body, Response, Headers).StatusCode;
end;

function TMistralAIAPI.Post(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
begin
  CheckAPI;
  Headers := GetHeaders;
  Stream := nil;
  try
    Result := FHTTPClient.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
  finally
  end;
end;

function TMistralAIAPI.Post<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TMistralAIAPI.Post<TParams>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean;
var
  Params: TParams;
  Code: Integer;
begin
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response, Event);
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
          ParseError(Code, Recieved.DataString);
        finally
          Recieved.Free;
        end;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TMistralAIAPI.Post<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Post(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TMistralAIAPI.Delete(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Delete(GetRequestURL(Path), Response, Headers).StatusCode;
end;

function TMistralAIAPI.Delete<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Delete(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TMistralAIAPI.PostForm<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

procedure TMistralAIAPI.RaiseError(Code: Int64; Error: TErrorCore);
begin
  case Code of
    429:
      raise MistralAIExceptionRateLimitError.Create(Code, Error);
    400, 404, 415:
      raise MistralAIExceptionInvalidRequestError.Create(Code, Error);
    401:
      raise MistralAIExceptionAuthenticationError.Create(Code, Error);
    403:
      raise MistralAIExceptionPermissionError.Create(Code, Error);
    409:
      raise MistralAIExceptionTryAgain.Create(Code, Error);
    422:
      raise MistralUnprocessableEntityError.Create(Code, Error);
  else
    raise MistralAIException.Create(Code, Error);
  end;
end;

function TMistralAIAPI.Get<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Get(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

procedure TMistralAIAPI.GetFile(const Path: string; Response: TStream);
var
  Headers: TNetHeaders;
  Code: Integer;
begin
  CheckAPI;
  Headers := GetHeaders;
  Code := FHTTPClient.Get(GetRequestURL(Path), Response, Headers).StatusCode;
  case Code of
    200..299:
      ; {success}
  else
    var Recieved := TStringStream.Create;
    try
      Response.Position := 0;
      Recieved.LoadFromStream(Response);
      ParseError(Code, Recieved.DataString);
    finally
      Recieved.Free;
    end;
  end;
end;

function TMistralAIAPI.GetHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('Authorization', 'Bearer ' + FToken)] + FCustomHeaders;
end;

function TMistralAIAPI.GetRequestURL(const Path: string): string;
begin
  Result := FBaseURL + '/';
  Result := Result + Path;
end;

procedure TMistralAIAPI.CheckAPI;
begin
  if FToken.IsEmpty then
    raise MistralAIExceptionAPI.Create('Token is empty!');
  if FBaseUrl.IsEmpty then
    raise MistralAIExceptionAPI.Create('Base url is empty!');
end;

procedure TMistralAIAPI.ParseError(const Code: Int64; const ResponseText: string);
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

function TMistralAIAPI.ParseResponse<T>(const Code: Int64; const ResponseText: string): T;
begin
  Result := nil;
  case Code of
    200..299:
      try
        Result := TJson.JsonToObject<T>(ResponseText);
      except
        Result := nil;
      end;
    else
      ParseError(Code, ResponseText);
  end;
  if not Assigned(Result) then
    raise MistralAIExceptionInvalidResponse.Create(Code, 'Empty or invalid response');
end;

procedure TMistralAIAPI.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

procedure TMistralAIAPI.SetCustomHeaders(const Value: TNetHeaders);
begin
  FCustomHeaders := Value;
end;

procedure TMistralAIAPI.SetOrganization(const Value: string);
begin
  FOrganization := Value;
end;

procedure TMistralAIAPI.SetToken(const Value: string);
begin
  FToken := Value;
end;

{ MistralAIException }

constructor MistralAIException.Create(const ACode: Int64; const AError: TErrorCore); //TError);
begin
  var Error := AError as TError;

  Code := ACode;
  if Error.Detail <> EmptyStr then
    Msg := Error.Detail
  else
  if Error.Message <> EmptyStr then
    Msg := Error.Message;
  if Error.RequestID <> EmptyStr then
    Msg := Format('%s for request %s', [Msg, Error.RequestID]);

  inherited Create(Format('error %d: %s', [ACode, Msg]));
end;

constructor MistralAIException.Create(const ACode: Int64; const Value: string);
begin
  Code := ACode;
  Msg := Value;
  inherited Create(Format('error %d: %s', [ACode, Msg]));
end;

{ TMistralAIAPIRoute }

constructor TMistralAIAPIRoute.CreateRoute(AAPI: TMistralAIAPI);
begin
  inherited Create;
  FAPI := AAPI;
end;

procedure TMistralAIAPIRoute.SetAPI(const Value: TMistralAIAPI);
begin
  FAPI := Value;
end;

{ MistralAIValidationException }

constructor MistralAIValidationException.Create(const ACode: Int64;
  const AError: TErrorCore);
begin
  var Error := AError as TError422;
  Code := ACode;
  if Length(Error.Detail) > 0 then
    inherited Create(Format('error %d: %s ', [ACode, TypeToStr(Error.Detail[0].Msg)]))
  else
    {--- WARNING ---
         Problem with errors generated by Mistral.
         (Detail & Message: inconsistent. Pending correction)}
    if Length(Error.Message.Detail) > 0 then
       inherited Create(
          Format('error %d: %s '+ sLineBreak +' %s',
                 [ACode, TypeToStr(Error.&Type), ToText(Error)]) )
    else
      inherited Create(Format('error %d: %s ', [ACode, 'Detail not captured']));
end;

function MistralAIValidationException.DetailItemToStr(
  const Value: TDetail): string;
begin
  Result := Format('   %s (%s) --- %s '+sLineBreak, [LocToStr(Value.Loc), Value.Input, Value.Msg])
end;

function MistralAIValidationException.LocToStr(
  const Value: TArray<string>): string;
begin
  Result := EmptyStr;
  for var S in Value do
    if IndexStr(S, ['body']) = -1 then
      begin
        if Result <> EmptyStr then
          Result := Result + '.' + S else
          Result := S;
      end;
end;

function MistralAIValidationException.ToText(
  const AError: TError422): string;
begin
  Result := EmptyStr;
  for var Item in AError.Message.Detail do
    begin
      if Result <> EmptyStr then
        Result := Format('%s %s', [Result, DetailItemToStr(Item)]) else
        Result := DetailItemToStr(Item);
    end;
end;

function MistralAIValidationException.TypeToStr(const Value: string): string;
begin
  Result := StringReplace(Value, '_', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

end.

