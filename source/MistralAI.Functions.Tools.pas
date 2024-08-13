unit MistralAI.Functions.Tools;

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  MistralAI.Functions.Core;

type
  TToolChoice = (
    /// <summary>
    /// The model won't call a function and will generate a message instead
    /// </summary>
    none,
    /// <summary>
    /// The model can choose to either generate a message or call a function
    /// </summary>
    auto,
    /// <summary>
    /// The model is forced to call a function
    /// </summary>
    any);

  TToolChoiceHelper = record helper for TToolChoice
    function ToString: string;
  end;

  TChatMessageTool = record
  private
    FFunction: IFunctionCore;
  public
    /// <summary>
    /// This method converts the TFunctionCore instance to a JSON object containing the type and
    /// representation of the function, and handles exceptions by deleting the JSON object and
    /// propagating the exception if an error occurs
    /// </summary>
    function ToJson: TJSONObject;
    /// <summary>
    /// The function properties
    /// </summary>
    property &Function: IFunctionCore read FFunction write FFunction;
    class function Add(const AFunction: IFunctionCore): TChatMessageTool; static;
  end;

  TCalledFunctionSpecifics = class
  private
    [JsonNameAttribute('name')]
    FName: string;
    [JsonNameAttribute('arguments')]
    FArguments: string;
  public
    /// <summary>
    /// Name of the called function
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Calculed Arguments for the called function
    /// </summary>
    property Arguments: string read FArguments write FArguments;
  end;

  TCalledFunction = class
  private
    [JsonNameAttribute('function')]
    FFunction: TCalledFunctionSpecifics;
  public
    /// <summary>
    /// Specifics of the called function
    /// </summary>
    property &Function: TCalledFunctionSpecifics read FFunction write FFunction;
    destructor Destroy; override;
  end;

implementation

{ TToolChoiceHelper }

function TToolChoiceHelper.ToString: string;
begin
  case Self of
    auto:
      Exit('auto');
    any:
      Exit('any');
    else
      Exit('none');
  end;
end;

{ TChatMessageTool }

class function TChatMessageTool.Add(
  const AFunction: IFunctionCore): TChatMessageTool;
begin
  Result.&Function := AFunction;
end;

function TChatMessageTool.ToJson: TJSONObject;
begin
  Result := FFunction.ToJson;
end;

{ TCalledFunction }

destructor TCalledFunction.Destroy;
begin
  if Assigned(FFunction) then
    FFunction.Free;
  inherited;
end;


end.

