unit MistralAI.API.Params;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.JSON, System.SysUtils, System.Types, System.RTTI,
  System.Generics.Collections, System.Threading, System.NetEncoding,
  REST.JsonReflect, REST.Json.Interceptors;

type
  /// <summary>
  /// Represents a base class for all classes obtained after deserialization.
  /// </summary>
  /// <remarks>
  /// This class is designed to store the raw JSON string returned by the API,
  /// allowing applications to access the original JSON response if needed.
  /// </remarks>
  TJSONFingerprint = class
  private
    FJSONResponse: string;
  public
    /// <summary>
    /// Gets or sets the raw JSON string returned by the API.
    /// </summary>
    /// <remarks>
    /// Typically, the API returns a single JSON string, which is stored in this property.
    /// </remarks>
    property JSONResponse: string read FJSONResponse write FJSONResponse;
  end;

  TJSONInterceptorStringToString = class(TJSONInterceptor)
    constructor Create; reintroduce;
  protected
    RTTI: TRttiContext;
  end;

type
  /// <summary>
  /// Represents a reference to a procedure that takes a single argument of type T and returns no value.
  /// </summary>
  /// <param name="T">
  /// The type of the argument that the referenced procedure will accept.
  /// </param>
  /// <remarks>
  /// This type is useful for defining callbacks or procedures that operate on a variable of type T, allowing for more flexible and reusable code.
  /// </remarks>
  TProcRef<T> = reference to procedure(var Arg: T);

  TJSONParam = class
  private
    FJSON: TJSONObject;
    procedure SetJSON(const Value: TJSONObject);
    function GetCount: Integer;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Key: string; const Value: string): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Integer): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Int64): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Extended): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Boolean): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TDateTime; Format: string): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TJSONValue): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TJSONParam): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<string>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<Integer>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<Int64>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<Extended>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam; overload; virtual;
    function GetOrCreateObject(const Name: string): TJSONObject;
    function GetOrCreate<T: TJSONValue, constructor>(const Name: string): T;
    procedure Delete(const Key: string); virtual;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
    function Detach: TJSONObject;
    property JSON: TJSONObject read FJSON write SetJSON;
    function ToJsonString(FreeObject: Boolean = False): string; virtual;
    function ToFormat(FreeObject: Boolean = False): string;
    function ToStringPairs: TArray<TPair<string, string>>;
    function ToStream: TStringStream;
  end;

  TUrlParam = class
  private
    FValue: string;
    procedure Check(const Name: string);
    function GetValue: string;
  public
    /// <summary>
    /// Adds a string parameter to the query string.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The value of the parameter.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name, Value: string): TUrlParam; overload; virtual;

    /// <summary>
    /// Adds an integer parameter to the query string.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The integer value of the parameter.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name: string; Value: Integer): TUrlParam; overload; virtual;

    /// <summary>
    /// Adds an integer 64 parameter to the query string.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The integer 64 value of the parameter.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name: string; Value: Int64): TUrlParam; overload; virtual;

    /// <summary>
    /// Adds a boolean parameter to the query string.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The boolean value of the parameter. It will be converted to "true" or "false".
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name: string; Value: Boolean): TUrlParam; overload; virtual;

    /// <summary>
    /// Adds a double parameter to the query string.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The double value of the parameter.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name: string; Value: Double): TUrlParam; overload; virtual;

    /// <summary>
    /// Adds an array of string values to the query string as a single parameter.
    /// </summary>
    /// <param name="Name">
    /// The name of the parameter.
    /// </param>
    /// <param name="Value">
    /// The array of string values to be added, joined by commas.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUrlParam</c>, allowing for method chaining.
    /// </returns>
    function Add(const Name: string; Value: TArray<string>): TUrlParam; overload; virtual;

    /// <summary>
    /// Gets the constructed query string with all parameters.
    /// </summary>
    /// <returns>
    /// The query string, prefixed with a question mark ("?") if parameters are present.
    /// </returns>
    property Value: string read GetValue;

    constructor Create; virtual;
  end;

  /// <summary>
  /// Represents a generic key-value parameter manager.
  /// </summary>
  /// <remarks>
  /// This class allows storing and retrieving various types of parameters as key-value pairs.
  /// It supports basic types (integers, strings, booleans, floating-point numbers), objects,
  /// as well as arrays of these types.
  /// </remarks>
  /// <example>
  ///   <code>
  ///     var Params: TParameters;
  ///     begin
  ///       Params := TParameters.Create;
  ///       Params.Add('Limit', 100)
  ///             .Add('Order', 'Asc')
  ///             .Add('IsEnabled', True);
  ///       if Params.Exists('Limit') then
  ///         ShowMessage(IntToStr(Params.GetInteger('Limit')));
  ///       Params.Free;
  ///     end;
  ///   </code>
  /// </example>
  TParameters = class
  private
    FParams: TDictionary<string, TValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AKey: string; const AValue: Integer): TParameters; overload;
    function Add(const AKey: string; const AValue: Int64): TParameters; overload;
    function Add(const AKey: string; const AValue: string): TParameters; overload;
    function Add(const AKey: string; const AValue: Single): TParameters; overload;
    function Add(const AKey: string; const AValue: Double): TParameters; overload;
    function Add(const AKey: string; const AValue: Boolean): TParameters; overload;
    function Add(const AKey: string; const AValue: TObject): TParameters; overload;
    function Add(const AKey: string; const AValue: TJSONObject): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<string>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<Integer>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<Int64>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<Single>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<Double>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<Boolean>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<TObject>): TParameters; overload;
    function Add(const AKey: string; const AValue: TArray<TJSONObject>): TParameters; overload;

    function GetInteger(const AKey: string; const ADefault: Integer = 0): Integer;
    function GetInt64(const AKey: string; const ADefault: Integer = 0): Integer;
    function GetString(const AKey: string; const ADefault: string = ''): string;
    function GetSingle(const AKey: string; const ADefault: Single = 0.0): Double;
    function GetDouble(const AKey: string; const ADefault: Double = 0.0): Double;
    function GetBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
    function GetObject(const AKey: string; const ADefault: TObject = nil): TObject;
    function GetJSONObject(const AKey: string): TJSONObject;

    function GetArrayString(const AKey: string): TArray<string>;
    function GetArrayInteger(const AKey: string): TArray<Integer>;
    function GetArrayInt64(const AKey: string): TArray<Int64>;
    function GetArraySingle(const AKey: string): TArray<Single>;
    function GetArrayDouble(const AKey: string): TArray<Double>;
    function GetArrayBoolean(const AKey: string): TArray<Boolean>;
    function GetArrayObject(const AKey: string): TArray<TObject>;
    function GetArrayJSONObject(const AKey: string): TArray<TJSONObject>;
    function GetJSONArray(const AKey: string): TJSONArray;

    function Exists(const AKey: string): Boolean;
    procedure ProcessParam(const AKey: string; ACallback: TProc<TValue>);
  end;

const
  DATE_FORMAT = 'YYYY-MM-DD';
  TIME_FORMAT = 'HH:NN:SS';
  DATE_TIME_FORMAT = DATE_FORMAT + ' ' + TIME_FORMAT;

implementation

uses
  System.DateUtils;

{ TJSONInterceptorStringToString }

constructor TJSONInterceptorStringToString.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ Fetch }

type
  Fetch<T> = class
    type
      TFetchProc = reference to procedure(const Element: T);
  public
    class procedure All(const Items: TArray<T>; Proc: TFetchProc);
  end;

{ Fetch<T> }

class procedure Fetch<T>.All(const Items: TArray<T>; Proc: TFetchProc);
var
  Item: T;
begin
  for Item in Items do
    Proc(Item);
end;

{ TJSONParam }

function TJSONParam.Add(const Key, Value: string): TJSONParam;
begin
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONValue): TJSONParam;
begin
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONParam): TJSONParam;
begin
  Add(Key, TJSONValue(Value.JSON.Clone));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TDateTime; Format: string): TJSONParam;
begin
  if Format.IsEmpty then
    Format := DATE_TIME_FORMAT;
  Add(Key, FormatDateTime(Format, System.DateUtils.TTimeZone.local.ToUniversalTime(Value)));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Boolean): TJSONParam;
begin
  Add(Key, TJSONBool.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Integer): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Extended): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam;
var
  JArr: TJSONArray;
begin
  JArr := TJSONArray.Create;
  Fetch<TJSONValue>.All(Value, JArr.AddElement);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam;
var
  JArr: TJSONArray;
  Item: TJSONParam;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
  try
    JArr.AddElement(Item.JSON);
    Item.JSON := nil;
  finally
    Item.Free;
  end;
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<Int64>): TJSONParam;
var
  JArr: TJSONArray;
  Item: Integer;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.Add(Item);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Int64): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<Extended>): TJSONParam;
var
  JArr: TJSONArray;
  Item: Extended;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.Add(Item);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<Integer>): TJSONParam;
var
  JArr: TJSONArray;
  Item: Integer;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.Add(Item);
  Add(Key, JArr);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<string>): TJSONParam;
var
  JArr: TJSONArray;
  Item: string;
begin
  JArr := TJSONArray.Create;
  for Item in Value do
    JArr.Add(Item);
  Add(Key, JArr);
  Result := Self;
end;

procedure TJSONParam.Clear;
begin
  FJSON.Free;
  FJSON := TJSONObject.Create;
end;

constructor TJSONParam.Create;
begin
  FJSON := TJSONObject.Create;
end;

procedure TJSONParam.Delete(const Key: string);
var
  Item: TJSONPair;
begin
  Item := FJSON.RemovePair(Key);
  if Assigned(Item) then
    Item.Free;
end;

destructor TJSONParam.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

function TJSONParam.Detach: TJSONObject;
begin
  Result := JSON;
  JSON := nil;
  var Task: ITask := TTask.Create(
    procedure()
    begin
      Sleep(30);
      TThread.Queue(nil,
      procedure
      begin
        Self.Free;
      end);
    end
  );
  Task.Start;
end;

function TJSONParam.GetCount: Integer;
begin
  Result := FJSON.Count;
end;

function TJSONParam.GetOrCreate<T>(const Name: string): T;
begin
  if not FJSON.TryGetValue<T>(Name, Result) then
  begin
    Result := T.Create;
    FJSON.AddPair(Name, Result);
  end;
end;

function TJSONParam.GetOrCreateObject(const Name: string): TJSONObject;
begin
  Result := GetOrCreate<TJSONObject>(Name);
end;

procedure TJSONParam.SetJSON(const Value: TJSONObject);
begin
  FJSON := Value;
end;

function TJSONParam.ToFormat(FreeObject: Boolean): string;
begin
  Result := FJSON.Format(4);
  if FreeObject then
    Free;
end;

function TJSONParam.ToJsonString(FreeObject: Boolean): string;
begin
  Result := FJSON.ToJSON;
  if FreeObject then
    Free;
end;

function TJSONParam.ToStream: TStringStream;
begin
  Result := TStringStream.Create;
  try
    Result.WriteString(ToJsonString);
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONParam.ToStringPairs: TArray<TPair<string, string>>;
begin
  for var Pair in FJSON do
    Result := Result + [TPair<string, string>.Create(Pair.JsonString.Value, Pair.JsonValue.AsType<string>)];
end;

{ TUrlParam }

function TUrlParam.Add(const Name, Value: string): TUrlParam;
begin
  Check(Name);
  var S := Format('%s=%s', [Name, TNetEncoding.URL.Encode(Value).Replace('+', '%20')]);
  if FValue.IsEmpty then
    FValue := S else
    FValue := FValue + '&' + S;
  Result := Self;
end;

function TUrlParam.Add(const Name: string; Value: Integer): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

function TUrlParam.Add(const Name: string; Value: Boolean): TUrlParam;
begin
  Result := Add(Name, BoolToStr(Value, true));
end;

function TUrlParam.Add(const Name: string; Value: Double): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

procedure TUrlParam.Check(const Name: string);
var
  Params: TArray<string>;
begin
  var Items := FValue.Split(['&']);
  FValue := EmptyStr;
  for var Item in Items do
    begin
      if not Item.StartsWith(Name + '=') then
        Params := Params + [Item];
    end;
  FValue := string.Join('&', Params);
end;

constructor TUrlParam.Create;
begin
  FValue := EmptyStr;
end;

function TUrlParam.GetValue: string;
var
  Params: TArray<string>;
begin
  var Items := FValue.Split(['&']);
  for var Item in Items do
    begin
      var SubStr := Item.Split(['=']);
      if Length(SubStr) <> 2 then
        raise Exception.CreateFmt('%s: Ivalid URL parameter.', [SubStr]);
      Params := Params + [
        TNetEncoding.URL.Encode(SubStr[0]).Replace('+', '%20') + '=' + SubStr[1] ];
    end;
  Result := string.Join('&', Params);
  if not Result.IsEmpty then
    Result := '?' + Result;
end;

function TUrlParam.Add(const Name: string; Value: TArray<string>): TUrlParam;
begin
  Result := Add(Name, string.Join(',', Value).Replace(#32, #0));
end;

function TUrlParam.Add(const Name: string; Value: Int64): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

{ TParameters }

function TParameters.Add(const AKey: string;
  const AValue: Boolean): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: Double): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: Integer): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey, AValue: string): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

constructor TParameters.Create;
begin
  inherited Create;
  FParams := TDictionary<string, TValue>.Create;
end;

destructor TParameters.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TParameters.Exists(const AKey: string): Boolean;
begin
  Result := FParams.ContainsKey(AKey.ToLower)
end;

function TParameters.GetArrayBoolean(const AKey: string): TArray<Boolean>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<Boolean>> then
    Result := LValue.AsType<TArray<Boolean>>
  else
    Result := [];
end;

function TParameters.GetArrayDouble(const AKey: string): TArray<Double>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<Double>> then
    Result := LValue.AsType<TArray<Double>>
  else
    Result := [];
end;

function TParameters.GetArrayInt64(const AKey: string): TArray<Int64>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<Int64>> then
    Result := LValue.AsType<TArray<Int64>>
  else
    Result := [];
end;

function TParameters.GetArrayInteger(const AKey: string): TArray<Integer>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<Integer>> then
    Result := LValue.AsType<TArray<Integer>>
  else
    Result := [];
end;

function TParameters.GetArrayJSONObject(
  const AKey: string): TArray<TJSONObject>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<TJSONObject>> then
    Result := LValue.AsType<TArray<TJSONObject>>
  else
    Result := nil;
end;

function TParameters.GetArrayObject(const AKey: string): TArray<TObject>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<TObject>> then
    Result := LValue.AsType<TArray<TObject>>
  else
    Result := [];
end;

function TParameters.GetArraySingle(const AKey: string): TArray<Single>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<Single>> then
    Result := LValue.AsType<TArray<Single>>
  else
    Result := [];
end;

function TParameters.GetArrayString(const AKey: string): TArray<string>;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TArray<string>> then
    Result := LValue.AsType<TArray<string>>
  else
    Result := [];
end;

function TParameters.GetBoolean(const AKey: string;
  const ADefault: Boolean): Boolean;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<Boolean> then
    Result := LValue.AsBoolean
  else
    Result := ADefault;
end;

function TParameters.GetDouble(const AKey: string;
  const ADefault: Double): Double;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<Double> then
    Result := LValue.AsType<Double>
  else
    Result := ADefault;
end;

function TParameters.GetInt64(const AKey: string;
  const ADefault: Integer): Integer;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<Int64> then
    Result := LValue.AsInt64
  else
    Result := ADefault;
end;

function TParameters.GetInteger(const AKey: string;
  const ADefault: Integer): Integer;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<Integer> then
    Result := LValue.AsInteger
  else
    Result := ADefault;
end;

function TParameters.GetJSONArray(const AKey: string): TJSONArray;
begin
  Result := TJSONArray.Create;
  for var Item in GetArrayJSONObject(AKey) do
    Result.Add(Item);
end;

function TParameters.GetJSONObject(const AKey: string): TJSONObject;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<TJSONObject> then
    Result := LValue.AsType<TJSONObject>
  else
    Result := nil;
end;

function TParameters.GetObject(const AKey: string;
  const ADefault: TObject): TObject;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsObject then
    Result := LValue.AsObject
  else
    Result := ADefault;
end;

function TParameters.GetSingle(const AKey: string;
  const ADefault: Single): Double;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<Single> then
    Result := LValue.AsType<Single>
  else
    Result := ADefault;
end;

function TParameters.GetString(const AKey, ADefault: string): string;
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) and LValue.IsType<string> then
    Result := LValue.AsString
  else
    Result := ADefault;
end;

procedure TParameters.ProcessParam(const AKey: string;
  ACallback: TProc<TValue>);
var
  LValue: TValue;
begin
  if FParams.TryGetValue(AKey.ToLower, LValue) then
    ACallback(LValue);
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<string>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<string>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<Integer>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<Integer>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<Double>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<Double>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<Boolean>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<Boolean>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TObject): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<Single>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<Single>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: Single): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<Int64>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<Int64>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: Int64): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<TObject>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<TObject>>(AValue));
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TJSONObject): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, AValue);
  Result := Self;
end;

function TParameters.Add(const AKey: string;
  const AValue: TArray<TJSONObject>): TParameters;
begin
  FParams.AddOrSetValue(AKey.ToLower, TValue.From<TArray<TJSONObject>>(AValue));
  Result := Self;
end;

end.

