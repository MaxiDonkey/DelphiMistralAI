unit MistralAI.Functions.Example;

interface

uses
  System.SysUtils, MistralAI.Functions.Core;

type
  TWeatherReportFunction = class(TFunctionCore)
  protected
    function GetDescription: string; override;
    function GetName: string; override;
    function GetParameters: string; override;
    function GetType: string; override;
  public
    function Execute(const Arguments: string): string; override;
  end;

implementation

uses
  System.StrUtils, System.JSON;

{ TWeatherReportFunction }

function TWeatherReportFunction.Execute(const Arguments: string): string;

  procedure AddToReport(const Value: TJSONObject;
    Temperature: Integer; Forecast: TArray<string>);
  begin
    Value.AddPair('temperature', TJSONNumber.Create(Temperature));
    Value.AddPair('forecast', TJSONArray.Create(Forecast[0], Forecast[1]));
  end;

begin
  Result := EmptyStr;
  var Location := EmptyStr;
  var UnitType := EmptyStr;

  {--- Parse arguments to retrieve parameters }
  var JSON := TJSONObject.ParseJSONValue(Arguments) as TJSONObject;
  try
    if Assigned(JSON) then
    try
      Location := JSON.GetValue('location', '');
      UnitType := JSON.GetValue('unit', '');
    finally
      JSON.Free;
    end;
  except
    Location := EmptyStr;
  end;

  {--- Stop the treatment if location is empty }
  if Location.IsEmpty then
    Exit;

  {--- Build the response }
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('location', Location);
    JSON.AddPair('unit', UnitType);
    case IndexStr(AnsiLowerCase(Location), [
      'paris', 'paris, 75',
      'marseille', 'marseille, 13']) of
      0,1 :
        AddToReport(JSON, 14, ['rainy', 'low visibility']);

      2,3 :
        AddToReport(JSON, 29, ['sunny', 'windy']);
    end;
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end; {Execute}

function TWeatherReportFunction.GetDescription: string;
begin
  Result := 'Get the current weather in a given location.';
end;

function TWeatherReportFunction.GetName: string;
begin
  Result := 'get_weather';
end;

function TWeatherReportFunction.GetParameters: string;
begin
  Result :=
    '{'+
    '"type": "object",'+
    '"properties": {'+
         '"location": {'+
             '"type": "string",'+
             '"description": "The city and department, e.g. Marseille, 13"'+
         '},'+
         '"unit": {'+
             '"type": "string",'+
             '"enum": ["celsius", "fahrenheit"]'+
         '}'+
     '},'+
     '"required": ["location"]'+
  '}';
end;

function TWeatherReportFunction.GetType: string;
begin
  Result := 'function';
end;

end.
