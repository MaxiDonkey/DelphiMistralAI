﻿unit MistralAI.HttpClientAPI;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.URLClient,
  System.Net.Mime, System.JSON, System.NetConsts,
  MistralAI.API.Params, MistralAI.HttpClientInterface;

type
  /// <summary>
  /// Provides an implementation of the <c>IHttpClientAPI</c> interface using Delphi's <c>THTTPClient</c>.
  /// </summary>
  /// <remarks>
  /// This class facilitates making HTTP requests such as GET, POST, DELETE, and PATCH
  /// by wrapping Delphi's <c>THTTPClient</c> and adhering to the <c>IHttpClientAPI</c> interface.
  /// It supports setting timeouts, proxy configurations, and handling response callbacks.
  /// </remarks>
  THttpClientAPI = class(TInterfacedObject, IHttpClientAPI)
  private
    FHttpClient: THttpClient;
    FCheckSettingsProc: TProc;
    procedure SetSendTimeOut(const Value: Integer);
    function GetSendTimeOut: Integer;
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetResponseTimeout: Integer;
    procedure SetResponseTimeout(const Value: Integer);
    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);
    procedure CheckAPISettings; virtual;
  public
    /// <summary>
    /// Sends an HTTP GET request to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the GET request to.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Get(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP GET request to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the GET request to.
    /// </param>
    /// <param name="Response">
    /// A stream to capture the binary response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Get(const URL: string; const Response: TStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP DELETE request to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the DELETE request to.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Delete(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP DELETE request with a JSON body to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to which the DELETE request is sent.
    /// </param>
    /// <param name="Body">
    /// A <c>TJSONObject</c> representing the JSON payload to include in the DELETE request body.
    /// </param>
    /// <param name="Response">
    /// A <c>TStringStream</c> instance used to capture the server's response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers (<c>TNetHeaders</c>) to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server in response to the request.
    /// </returns>
    /// <remarks>
    /// This overload is useful when the DELETE request requires additional data
    /// in the body, such as filter criteria or resource identifiers.
    /// The JSON body is automatically serialized from the provided <c>TJSONObject</c>.
    /// </remarks>
    function Delete(const URL: string; Body: TJSONObject; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP POST request to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the POST request to.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Post(const URL: string; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP POST request with multipart form data to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the POST request to.
    /// </param>
    /// <param name="Body">
    /// The multipart form data to include in the POST request.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Post(const URL: string; Body: TMultipartFormData; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP POST request with a JSON body to the specified URL and handles streamed responses.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the POST request to.
    /// </param>
    /// <param name="Body">
    /// The JSON object to include in the POST request body.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <param name="OnReceiveData">
    /// A callback procedure to handle data as it is received during the streaming process.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Post(const URL: string; Body: TJSONObject; Response: TStringStream; const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback): Integer; overload;

    /// <summary>
    /// Sends an HTTP POST request with a JSON body to the specified URL and handles a full streamed responses.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the POST request to.
    /// </param>
    /// <param name="Body">
    /// The JSON object to include in the POST request body.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <param name="OnReceiveData">
    /// A callback procedure to handle data as it is received during the streaming process.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Post(const URL: string; Body: TJSONObject; Response: TStream; const Headers: TNetHeaders; OnReceiveData: TReceiveDataCallback): Integer; overload;

    /// <summary>
    /// Sends an HTTP PATCH request with a JSON body to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the PATCH request to.
    /// </param>
    /// <param name="Body">
    /// The JSON object to include in the PATCH request body.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Patch(const URL: string; Body: TJSONObject; Response: TStringStream; const Headers: TNetHeaders): Integer; overload;

    /// <summary>
    /// Sends an HTTP PUT request with a JSON body to the specified URL.
    /// </summary>
    /// <param name="URL">
    /// The endpoint URL to send the PUT request to.
    /// </param>
    /// <param name="Body">
    /// The JSON object to include in the PUT request body. It will be encoded as UTF‑8.
    /// </param>
    /// <param name="Response">
    /// A string stream to capture the response content.
    /// </param>
    /// <param name="Headers">
    /// A list of HTTP headers to include in the request.
    /// </param>
    /// <returns>
    /// The HTTP status code returned by the server.
    /// </returns>
    function Put(const URL: string; Body: TJSONObject; Response: TStringStream; const Headers: TNetHeaders): Integer;

    /// <summary>
    /// Initializes a new instance of the <c>THttpClientAPI</c> class.
    /// </summary>
    /// <param name="CheckProc">
    /// A callback procedure to verify API settings before each request.
    /// </param>
    constructor Create(const CheckProc: TProc);

   /// <summary>
    /// Creates and returns an instance of <c>IHttpClientAPI</c>.
    /// </summary>
    /// <param name="CheckProc">
    /// A callback procedure to verify API settings before each request.
    /// </param>
    /// <returns>
    /// An instance implementing the <c>IHttpClientAPI</c> interface.
    /// </returns>
    class function CreateInstance(const CheckProc: TProc): IHttpClientAPI;

    destructor Destroy; override;
  end;

implementation

{ THttpClientAPI }

procedure THttpClientAPI.CheckAPISettings;
begin
  if Assigned(FCheckSettingsProc) then
    FCheckSettingsProc;
end;

constructor THttpClientAPI.Create(const CheckProc: TProc);
begin
  inherited Create;
  FHttpClient := THTTPClient.Create;
  FHttpClient.AcceptCharSet := 'utf-8';
  FCheckSettingsProc := CheckProc;
end;

class function THttpClientAPI.CreateInstance(
  const CheckProc: TProc): IHttpClientAPI;
begin
  Result := THttpClientAPI.Create(CheckProc);
end;

function THttpClientAPI.Delete(const URL: string;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Delete(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.Delete(const URL: string; Body: TJSONObject;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  var Stream := TStringStream.Create;
  try
    Stream.WriteString(Body.ToJSON);
    Stream.Position := 0;
    Result := FHttpClient.Delete(URL, Stream, Response, Headers).StatusCode;
  finally
    Stream.Free;
  end;
end;

destructor THttpClientAPI.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function THttpClientAPI.Get(const URL: string; const Response: TStream;
  const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Get(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.GetConnectionTimeout: Integer;
begin
  Result := FHttpClient.ConnectionTimeout;
end;

function THttpClientAPI.GetProxySettings: TProxySettings;
begin
  Result := FHttpClient.ProxySettings;
end;

function THttpClientAPI.GetResponseTimeout: Integer;
begin
  Result := FHttpClient.ResponseTimeout;
end;

function THttpClientAPI.GetSendTimeOut: Integer;
begin
  Result := FHttpClient.SendTimeout;
end;

function THttpClientAPI.Get(const URL: string;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Get(URL, Response, Headers).StatusCode;
end;

function THttpClientAPI.Patch(const URL: string; Body: TJSONObject;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  var Stream := TStringStream.Create;
  try
    if Assigned(Body) then
      begin
        Stream.WriteString(Body.ToJSON);
        Stream.Position := 0;
      end;
    Result := FHttpClient.Patch(URL, Stream, Response, Headers).StatusCode;
  finally
    Stream.Free;
  end;
end;

function THttpClientAPI.Post(const URL: string; Body: TJSONObject;
  Response: TStringStream; const Headers: TNetHeaders;
  OnReceiveData: TReceiveDataCallback): Integer;
begin
  CheckAPISettings;
  var Stream := TStringStream.Create;
  FHttpClient.ReceiveDataCallBack := OnReceiveData;
  try
    Stream.WriteString(Body.ToJSON);
    Stream.Position := 0;
    Result := FHttpClient.Post(URL, Stream, Response, Headers).StatusCode;
  finally
    FHttpClient.ReceiveDataCallBack := nil;
    Stream.Free;
  end;
end;

procedure THttpClientAPI.SetConnectionTimeout(const Value: Integer);
begin
  FHttpClient.ConnectionTimeout := Value;
end;

procedure THttpClientAPI.SetProxySettings(const Value: TProxySettings);
begin
  FHttpClient.ProxySettings := Value;
end;

procedure THttpClientAPI.SetResponseTimeout(const Value: Integer);
begin
  FHttpClient.ResponseTimeout := Value;
end;

procedure THttpClientAPI.SetSendTimeOut(const Value: Integer);
begin
  FHttpClient.SendTimeout := Value;
end;

function THttpClientAPI.Post(const URL: string; Body: TMultipartFormData;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  Result := FHttpClient.Post(URL, Body, Response, Headers).StatusCode;
end;

function THttpClientAPI.Post(const URL: string; Response: TStringStream;
  const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;
  var Stream: TStringStream := nil;
  Result := FHttpClient.Post(URL, Stream, Response, Headers).StatusCode;
end;

function THttpClientAPI.Post(const URL: string; Body: TJSONObject;
  Response: TStream; const Headers: TNetHeaders;
  OnReceiveData: TReceiveDataCallback): Integer;
begin
  CheckAPISettings;

  {--- Query always encoded in explicit UTF-8 }
  var Bytes := TEncoding.UTF8.GetBytes(Body.ToJSON);
  var Req := TMemoryStream.Create;
  Req.WriteBuffer(Bytes, Length(Bytes));
  Req.Position := 0;

  FHttpClient.ReceiveDataCallback := OnReceiveData;
  try
    Result := FHttpClient.Post(URL, Req, Response, Headers).StatusCode;
  finally
    FHttpClient.ReceiveDataCallback := nil;
    Req.Free;
  end;
end;

function THttpClientAPI.Put(const URL: string; Body: TJSONObject;
  Response: TStringStream; const Headers: TNetHeaders): Integer;
begin
  CheckAPISettings;

  {--- Query always encoded in explicit UTF-8 }
  var Bytes := TEncoding.UTF8.GetBytes(Body.ToJSON);
  var Req := TMemoryStream.Create;
  Req.WriteBuffer(Bytes, Length(Bytes));
  Req.Position := 0;

  try
    Result := FHttpClient.Put(URL, Req, Response, Headers).StatusCode;
  finally
    Req.Free;
  end;
end;

end.
