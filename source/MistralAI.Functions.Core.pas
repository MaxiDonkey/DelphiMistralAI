unit MistralAI.Functions.Core;

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  IFunctionCore = interface
    ['{2D8EA8C5-1E60-481D-B95F-2EFFE2CF6A7D}']
    function GetDescription: string;
    function GetName: string;
    function GetParameters: string;
    function GetType: string;
    function Execute(const Arguments: string): string;
    /// <summary>
    /// This method converts the TFunctionCore instance to a JSON object containing the type and
    /// representation of the function, and handles exceptions by deleting the JSON object and
    /// propagating the exception if an error occurs
    /// </summary>
    function ToJson: TJSONObject;
    /// <summary>
    /// This method composes a string representation of the TFunctionCore instance, including
    /// its description, name and parameters, formatted in JSON style
    /// </summary>
    function ToString: string;
    /// <summary>
    /// A summary of the function's purpose, utilized by the model to determine the timing and method
    /// for invoking the function.
    /// </summary>
    property Description: string read GetDescription;
    /// <summary>
    /// The identifier for the function that will be invoked. It should only include characters from
    /// a-z, A-Z, 0-9, underscores, or dashes, and must not exceed 64 characters in length.
    /// </summary>
    property Name: string read GetName;
    /// <summary>
    /// The arguments that the function requires, defined using a JSON Schema object. Refer to the guide
    /// for sample implementations and consult the JSON Schema documentation for details on formatting.
    /// For a function that does not accept any parameters, use the specification
    /// {"type": "object", "properties": {}}
    /// </summary>
    property Parameters: string read GetParameters;
    /// <summary>
    /// The type of the tool. Currently, only "function" is supported
    /// </summary>
    property &Type: string read GetType;
  end;

  TFunctionCore = class abstract(TinterfacedObject, IFunctionCore)
  private
    FDescription: string;
    FName: string;
    FParameters: string;
  protected
    function GetDescription: string; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetParameters: string; virtual; abstract;
    function GetType: string; virtual; abstract;
  public
    function Execute(const Arguments: string): string; virtual; abstract;
    /// <summary>
    /// This method converts the TFunctionCore instance to a JSON object containing the type and
    /// representation of the function, and handles exceptions by deleting the JSON object and
    /// propagating the exception if an error occurs
    /// </summary>
    function ToJson: TJSONObject;
    /// <summary>
    /// This method composes a string representation of the TFunctionCore instance, including
    /// its description, name and parameters, formatted in JSON style
    /// </summary>
    function ToString: string; override;
    /// <summary>
    /// A summary of the function's purpose, utilized by the model to determine the timing and method
    /// for invoking the function.
    /// </summary>
    property Description: string read GetDescription;
    /// <summary>
    /// The identifier for the function that will be invoked. It should only include characters from
    /// a-z, A-Z, 0-9, underscores, or dashes, and must not exceed 64 characters in length.
    /// </summary>
    property Name: string read GetName;
    /// <summary>
    /// The arguments that the function requires, defined using a JSON Schema object. Refer to the guide
    /// for sample implementations and consult the JSON Schema documentation for details on formatting.
    /// For a function that does not accept any parameters, use the specification
    /// {"type": "object", "properties": {}}
    /// </summary>
    property Parameters: string read GetParameters;
    /// <summary>
    /// The type of the tool. Currently, only "function" is supported
    /// </summary>
    property &Type: string read GetType;
  end;

implementation

{ TFunctionCore }

function TFunctionCore.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('type', &Type);
    Result.AddPair('function', TJSONObject.ParseJSONValue(ToString));
  except
    on E: Exception do
      begin
        Result.Free;
        raise;
      end;
  end;
end;

function TFunctionCore.ToString: string;
begin
  with TStringWriter.Create do
  try
    Write('"description": "%s",', [Description]);
    Write('"name": "%s",', [Name]);
    Write('"parameters": %s', [Parameters]);
    Result := Format('{%s}', [ToString]);
  finally
    Free;
  end;
end;

end.
